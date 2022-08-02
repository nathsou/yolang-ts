import { Maybe, none, some } from "../utils/maybe";
import { Ref, ref, snd } from "../utils/misc";
import { error, ok, Result } from "../utils/result";
import { Slice } from "../utils/slice";
import { RecoveryStrategy } from "./recovery";
import { Keyword, Symbol, Token, TokenWithPos } from "./token";

// Syntax Error Recovery in Parsing Expression Grammars - https://arxiv.org/abs/1806.11150

export type Parser<T> = Ref<(tokens: Slice<Token>) => ParserResult<T>>;

export type ParserResult<T> = [res: Result<T, ParserError>, remaining: Slice<Token>, errors: ParserError[]];

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

const fail: ParserError['message'] = '<fail>';

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
    Some: h => {
      const res = f(h);
      return [
        res.match({ Some: x => ok(x), None: () => error({ message: fail, pos: tokens.start }) }),
        res.isSome() ? Slice.tail(tokens) : tokens,
        []
      ];
    },
    None: () => [error({ message: fail, pos: tokens.start }), tokens, []],
  }));
};

export const token = (token: Token): Parser<Token> => {
  return satisfy(t => Token.eq(t, token));
};

export const symbol = (symb: Symbol) => token(Token.Symbol(symb));
export const keyword = (keyword: Keyword) => token(Token.Keyword(keyword));

export const map = <T, U>(p: Parser<T>, f: (t: T, tokens: Slice<Token>) => U): Parser<U> => {
  return ref(tokens => {
    const [t, rem, errs] = p.ref(tokens);
    return [t.map(x => f(x, tokens)), rem, errs];
  });
};

export const flatMap = <T, U>(p: Parser<T>, f: (t: T, tokens: Slice<Token>) => Parser<U>): Parser<U> => {
  return ref(tokens => {
    const [t, rem, errs] = p.ref(tokens);
    return t.match({
      Ok: t => f(t, rem).ref(rem),
      Error: err => [error({ message: fail, pos: rem.start }), rem, [...errs, err]],
    });
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
  return ref(tokens => {
    const originalTokens = tokens;
    const errors: ParserError[] = [];
    const tuple = [];

    for (const p of parsers) {
      const [t, rem, errs] = p.ref(tokens);
      errors.push(...errs);
      tokens = rem;

      if (t.isError()) {
        return [t, originalTokens, errors];
      }

      tuple.push(t.unwrap());
    }

    return [ok(tuple), tokens, errors];
  });
};

export const alt = <T>(...parsers: Parser<T>[]): Parser<T> => {
  return ref(tokens => {
    const errors: ParserError[] = [];

    for (const p of parsers) {
      const [t, rem, errs] = p.ref(tokens);
      errors.push(...errs);

      if (t.raw.type === 'ok') {
        return [t, rem, errors];
      }

      if (t.raw.data.message !== fail) {
        return [t, tokens, errors];
      }
    }

    return [error({ message: fail, pos: tokens.start }), tokens, errors];
  });
};

export const many = <T>(p: Parser<T>): Parser<T[]> => {
  return ref(tokens => {
    const errors: ParserError[] = [];
    const ts: T[] = [];
    let isFirst = true;

    while (true) {
      const [t, rem, errs] = p.ref(tokens);
      errors.push(...errs);
      tokens = rem;

      if (t.raw.type === 'error') {
        if (isFirst && t.raw.data.message !== fail) {
          return [error(t.raw.data), tokens, errors];
        }

        return [ok(ts), tokens, errors];
      }

      ts.push(t.unwrap());
      isFirst = false;
    }
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

export const optionalOrDefault = <T>(p: Parser<T>, def: T): Parser<T> => {
  return map(optional(p), m => m.orDefault(def));
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

export const not = <T>(p: Parser<T>, { consume } = { consume: true }): Parser<null> => {
  return ref(tokens => {
    const [t, rem] = p.ref(tokens);
    return t.match({
      Ok: () => [error({ message: fail, pos: tokens.start }), consume ? rem : tokens, []], // not.2
      Error: () => [ok(null), consume ? rem : tokens, []], // not.1
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
): Parser<T> => {
  return ref(tokens => {
    const [t, rem, errs] = p.ref(tokens);
    return t.match({
      Ok: t => [ok(t), rem, errs],
      Error: e => {
        const [_, rem2, errs2] = recover({
          message,
          pos: e.pos,
          recovery: recovery ?? e.recovery
        }).ref(tokens);

        return [error({ message: fail, pos: tokens.start }), rem2, errs2];
      },
    });
  });
};

export const expectOrDefault = <T>(
  p: Parser<T>,
  message: ParserError['message'],
  defaultValue: T | ((message: string) => T)
): Parser<T> => {
  return ref(tokens => {
    const [t, rem, errs] = p.ref(tokens);
    return t.match({
      Ok: t => [ok(t), rem, errs],
      Error: () => [
        ok(typeof defaultValue === 'function' ? (defaultValue as any)(message) : defaultValue),
        rem,
        errs
      ]
    });
  });
};

export const conditionalError = <T>(parser: Parser<T>, pred: (data: T) => boolean, message: ParserError['message']): Parser<T> => {
  return ref(tokens => {
    const [t, rem, errs] = parser.ref(tokens);
    return t.match({
      Ok: t => {
        if (!pred(t)) {
          return [error({ message, pos: tokens.start }), rem, [...errs, { message, pos: tokens.start }]];
        } else {
          return [ok(t), rem, errs];
        }
      },
      Error: e => [error(e), rem, errs],
    });
  });
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
        if (lastToken.variant === 'EOF') {
          return [t, rem, errs];
        }

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

export const effect = <T>(p: Parser<T>, action: (data: T, tokens: Slice<Token>) => void): Parser<T> => {
  return map(p, (data, tokens) => {
    action(data, tokens);
    return data;
  });
};

export const parens = nestedBy('(', ')');
export const squareBrackets = nestedBy('[', ']');
export const curlyBrackets = nestedBy('{', '}');
export const angleBrackets = nestedBy('<', '>');