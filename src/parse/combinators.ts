import { Maybe, none, some } from "../utils/maybe";
import { Ref, ref, snd } from "../utils/misc";
import { error, ok, Result } from "../utils/result";
import { Slice } from "../utils/slice";
import { RecoveryStrategy } from "./recovery";
import { Keyword, Symbol, Token, TokenWithPos } from "./token";

// Syntax Error Recovery in Parsing Expression Grammars - https://arxiv.org/abs/1806.11150

export type ParserError = {
  message: string,
  pos: number,
  recovery?: RecoveryStrategy,
};

export const uninitialized = <T>(): Parser<T> => {
  return ref(tokens => [error({ message: 'uninitialized', pos: tokens.start }), tokens, []]);
};

export const initParser = <T>(l: Parser<T>, r: Parser<T>): void => {
  l.ref = r.ref;
};

export const formatError = (error: ParserError, tokens: TokenWithPos[]): string => {
  const { pos } = tokens[error.pos];
  return `${error.message} at ${pos.line}:${pos.column}`;
};

export type ParserResult<T> = [res: Result<T, ParserError>, remaining: Slice<Token>, errors: ParserError[]];

const fail: ParserError['message'] = '<fail>';

const farthest = <T>(a: Slice<T>, b: Slice<T>) => a.start > b.start ? a : b;

export type Parser<T> = Ref<(tokens: Slice<Token>) => ParserResult<T>>;

export const satisfy = (pred: (t: Token) => boolean): Parser<Token> => {
  return ref(tokens => {
    const err = error<Token, ParserError>({
      message: fail,
      pos: tokens.start,
    });

    return Slice.head(tokens).match({
      Some: t => pred(t) ? [ok(t), Slice.tail(tokens), []] : [err, tokens, []],
      None: () => [err, tokens, []],
    });
  });
};

export const satisfyBy = <T>(f: (t: Token) => Maybe<T>): Parser<T> => {
  return ref(tokens => Slice.head(tokens).match({
    Some: h => [
      f(h).match({ Some: x => ok(x), None: () => error({ message: fail, pos: tokens.start }) }),
      Slice.tail(tokens),
      []
    ],
    None: () => [error({ message: fail, pos: tokens.start }), tokens, []],
  }));
};

export const token = (token: Token): Parser<Token> => {
  return satisfy(t => Token.eq(t, token));
};

export const symbol = (symb: Symbol) => token(Token.symbol(symb));
export const keyword = (keyword: Keyword) => token(Token.keyword(keyword));

export const map = <T, U>(p: Parser<T>, f: (t: T, tokens: Slice<Token>) => U): Parser<U> => {
  return ref(tokens => {
    const [t, rem, errs] = p.ref(tokens);
    return [t.map(x => f(x, tokens)), rem, errs];
  });
};

export const flatMap = <T, U>(p: Parser<T>, f: (t: T, tokens: Slice<Token>) => Result<U, ParserError>): Parser<U> => {
  return ref(tokens => {
    const [t, rem, errs] = p.ref(tokens);
    return [t.flatMap(x => f(x, rem)), rem, errs];
  });
};

export const mapParserResult = <T, U>(p: Parser<T>, f: (r: ParserResult<T>) => ParserResult<U>): Parser<U> => {
  return ref(tokens => {
    return f(p.ref(tokens));
  });
};

type MapP<Ts extends readonly any[]> =
  Ts extends [Parser<infer A>, ...infer As] ?
  [A, ...MapP<As>] :
  Ts extends [Parser<infer A>] ? [A] : [];

export const seq = <T extends readonly Parser<any>[]>(...parsers: T): Parser<[...MapP<T>]> => {
  if (parsers.length === 0) {
    return ref(tokens => [ok([] as [...MapP<T>]), tokens, []]);
  }

  const [p, ...ps] = parsers;

  return ref(tokens => {
    const [a, rema, errsa] = p.ref(tokens);
    return a.match({
      Ok: a => {
        const [b, remb, errsb] = seq(...ps).ref(rema);
        return b.match({
          Ok: b => [ok([a, ...b] as [...MapP<T>]), remb, [...errsa, ...errsb]], // seq.1
          Error: err => [error(err), rema, [...errsa, ...errsb]], // seq.2 & seq.3
        });
      },
      Error: err => [error(err), rema, errsa], // seq.4
    });
  });
};

export const alt = <T>(...parsers: Parser<T>[]): Parser<T> => {
  return ref(tokens => {
    if (parsers.length === 0) {
      return [error({ message: fail, pos: tokens.start }), tokens, []];
    }

    const [p, ...ps] = parsers;
    const [t, rem, errs] = p.ref(tokens);
    return t.match({
      Ok: t => [ok(t), rem, errs], // ord.1
      Error: l1 => {
        if (l1.message !== fail) {
          return [error(l1), rem, errs]; // ord.2
        } else {
          const [t2, rem2, errs2] = alt(...ps).ref(tokens);
          return t2.match({
            Ok: t2 => [ok(t2), rem2, [...errs, ...errs2]], // ord.4
            Error: l2 => {
              if (l2.message !== fail) {
                return [error(l2), rem, [...errs, ...errs2]]; // ord.5
              } else {
                // ord.3
                return [error({ message: fail, pos: tokens.start }), tokens, [...errs, ...errs2]];
              }
            },
          });
        }
      }
    });
  });
};

export const many = <T>(p: Parser<T>): Parser<T[]> => {
  return ref(tokens => {
    const [t, rem, errs] = p.ref(tokens);
    if (Slice.isEmpty(tokens)) {
      return [t.map(t => [t]), rem, errs];
    }

    return t.match({
      Ok: t => {
        const [ts, rem2, errs2] = many(p).ref(rem);
        return ts.match({
          Ok: ts => [ok([t, ...ts]), rem2, [...errs, ...errs2]], // rep.2
          Error: () => [ok([t]), rem2, errs], // rep.4
        });
      },
      Error: l => {
        if (l.message === fail) {
          return [ok([]), rem, errs]; // rep.1
        } else {
          return [error(l), rem, errs]; // rep.3
        }
      },
    });
  });
};

export const oneOrMore = <T>(p: Parser<T>): Parser<T[]> => {
  return map(seq(p, many(p)), ([h, tl]) => [h, ...tl]);
};

export const optional = <T>(p: Parser<T>): Parser<Maybe<T>> => {
  return ref(tokens => {
    const [t, rem, errs] = p.ref(tokens);
    return t.match({
      Ok: t => [ok(some(t)), rem, errs],
      Error: () => [ok(none), rem, errs],
    });
  });
};

export const leftAssoc = <L, R>(
  left: Parser<L>,
  right: Parser<R>,
  combine: (left: L, right: R) => L
): Parser<L> => {
  return map(
    seq(left, many(right)),
    ([h, tl]) => tl.length === 0 ? h : tl.reduce(combine, h)
  );
};

export const chainLeft = <L, Op, R>(
  l: Parser<L>,
  op: Parser<Op>,
  r: Parser<R>,
  combine: (lhs: L, op: Op, rhs: R) => L
): Parser<L> => {
  return leftAssoc(l, seq(op, r), (lhs, [op, rhs]) => combine(lhs, op, rhs));
};

export const sepBy = <S>(sep: Parser<S>) => <T>(p: Parser<T>): Parser<T[]> => {
  return map(seq(p, many(seq(sep, p))), ([h, tl]) => [h, ...tl.map(snd)]);
};

export const commas = sepBy(symbol(','));
export const semicolons = sepBy(symbol(';'));

export const not = <T>(p: Parser<T>): Parser<null> => {
  return ref(tokens => {
    const [t, rem] = p.ref(tokens);
    return t.match({
      Ok: () => [error({ message: fail, pos: tokens.start }), rem, []], // not.2
      Error: () => [ok(null), rem, []], // not.1
    });
  });
};

export const recover = (err: ParserError): Parser<null> => {
  return ref(tokens => {
    if (err.recovery === undefined) {
      return [error(err), tokens, [err]]; // throw.1
    }

    const recoveredRem = err.recovery(tokens);
    return [ok(null), recoveredRem, [err]]; // throw.2
  });
};

export const expect = <T>(
  p: Parser<T>,
  message: ParserError['message'],
  recovery?: ParserError['recovery'],
): Parser<Maybe<T>> => {
  return ref(tokens => {
    const [t, rem, errs] = p.ref(tokens);
    return t.match({
      Ok: t => [ok(some(t)), rem, errs],
      Error: e => {
        const [_, rem2, errs2] = recover({
          message,
          pos: e.pos,
          recovery: recovery ?? e.recovery
        }).ref(tokens);

        return [ok(none), rem2, errs2];
      },
    });
  });
};

export const expectOrDefault = <T>(
  p: Parser<T>,
  message: ParserError['message'],
  defaultValue: T | ((message: string) => T),
  recovery?: ParserError['recovery']
): Parser<T> => {
  const defaultVal: T = typeof defaultValue === 'function' ? (defaultValue as any)(message) : defaultValue;

  return map(
    expect(p, message, recovery),
    maybe => maybe.orDefault(defaultVal)
  );
};

export const lookahead = (check: (tokens: Slice<Token>) => boolean): Parser<null> => {
  return ref(tokens => {
    if (check(tokens)) {
      return [ok(null), tokens, []];
    } else {
      return [error({ message: fail, pos: tokens.start }), tokens, []];
    }
  });
};

export const consumeAll = <T>(p: Parser<T>): Parser<T> => {
  return ref(tokens => {
    const [t, rem, errs] = p.ref(tokens);

    return Slice.head(rem).match({
      Some: lastToken => {
        const err: ParserError = {
          message: `Unexpected token: '${Token.show(lastToken)}'`,
          pos: rem.start
        };

        return t.match({
          Ok: () => [
            error(err),
            Slice.tail(rem),
            errs
          ],
          Error: e => [
            error(err),
            rem,
            [...errs, e, err]
          ],
        });
      },
      None: () => [t, rem, errs]
    });
  });
};

export const nestedBy = (left: Symbol, right: Symbol) => <T>(p: Parser<T>): Parser<T> => {
  return map(
    seq(
      symbol(left),
      p,
      expect(symbol(right), `Expected closing '${right}'`, RecoveryStrategy.skipNested(left, right))
    ),
    ([, t,]) => t
  );
};

export const parens = nestedBy('(', ')');
export const squareBrackets = nestedBy('[', ']');
export const curlyBrackets = nestedBy('{', '}');