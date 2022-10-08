import { match, VariantOf } from "itsamatch";
import { Expr } from "../ast/sweet";
import { Error } from "../errors/errors";
import { Maybe, none, some } from "../utils/maybe";
import { panic, Ref, ref, snd } from "../utils/misc";
import { error, ok, Result } from "../utils/result";
import { Slice } from "../utils/slice";
import { isLowerCase, isUpperCase } from "../utils/strings";
import { RecoveryStrategy } from "./recovery";
import { Keyword, Position, SpecialOperator, Symbol, Token } from "./token";

// Syntax Error Recovery in Parsing Expression Grammars - https://arxiv.org/abs/1806.11150

export type Parser<T> = Ref<(tokens: Slice<Token>) => ParserResult<T>>;

export type ParserResult<T> = [res: Result<T, Error>, remaining: Slice<Token>, errors: Error[]];

export type ParserError = {
  message: string,
  recovery?: RecoveryStrategy,
};

export const lexerContext = {
  tokens: new Array<Token>(),
};

export const pos = (index: number): Position => {
  if (index >= lexerContext.tokens.length) {
    return panic(`invalid token index: ${index} / ${lexerContext.tokens.length}`);
  }

  return lexerContext.tokens[index].pos;
};

export const uninitialized = <T>(): Parser<T> => {
  return ref(tokens => [error(Error.Parser({ message: 'uninitialized' }, pos(tokens.start))), tokens, []]);
};

export const initParser = <T>(l: Parser<T>, r: Parser<T>): void => {
  l.ref = r.ref;
};

const fail: ParserError['message'] = '<fail>';

export const satisfy = (pred: (t: Token) => boolean): Parser<Token> => {
  return ref(tokens => {
    const err = error<Token, Error>(Error.Parser({ message: fail, }, pos(tokens.start)));

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
        res.match({ Some: x => ok(x), None: () => error(Error.Parser({ message: fail }, pos(tokens.start))) }),
        res.isSome() ? Slice.tail(tokens) : tokens,
        []
      ];
    },
    None: () => [error(Error.Parser({ message: fail }, pos(tokens.start))), tokens, []],
  }));
};

export const token = (token: Token): Parser<Token> => {
  return satisfy(t => Token.eq(t, token));
};

export const symbol = (symb: Symbol) => token(Token.Symbol(symb));
export const keyword = (keyword: Keyword) => token(Token.Keyword(keyword));

export const anyIdent = satisfyBy<string>(token => match(token, {
  Identifier: ({ name }) => isLowerCase(name[0]) ? some(name) : none,
  _: () => none
}));

export const anyOperator = satisfyBy<string>(token => match(token, {
  Operator: ({ value }) => some(value),
  _: () => none
}));

export const anyKeyword = satisfyBy<string>(token => match(token, {
  Keyword: ({ value }) => some(value),
  _: () => none
}));

export const ident = <S extends string>(...names: S[]): S extends SpecialOperator | Keyword ? never : Parser<S> => {
  return map(
    satisfy(token => token.variant === 'Identifier' && names.includes(token.name as S)),
    token => (token as VariantOf<Token, 'Identifier'>).name as S
  ) as any;
};

export const operator = <S extends SpecialOperator>(...names: S[]) => map(
  satisfy(token => token.variant === 'Operator' && names.includes(token.value as S)),
  token => (token as VariantOf<Token, 'Operator'>).value as S
);

export const upperIdent = satisfyBy<string>(token => match(token, {
  Identifier: ({ name }) => isUpperCase(name[0]) ? some(name) : none,
  _: () => none
}));

export const angleBrackets = <T>(p: Parser<T>) => map(
  seq(
    operator('<'),
    p,
    expect(operator('>'), `expected closing '>'`),
  ),
  ([_1, r, _2]) => r
);

export const map = <T, U>(p: Parser<T>, f: (t: T, pos: Position, tokens: Slice<Token>) => U): Parser<U> => {
  return ref(tokens => {
    const [t, rem, errs] = p.ref(tokens);
    const pos = Slice.head(tokens).mapWithDefault(t => t.pos, Position.DONT_CARE);
    return [t.map(x => f(x, pos, tokens)), rem, errs];
  });
};

export const flatMap = <T, U>(p: Parser<T>, f: (t: T, tokens: Slice<Token>) => Parser<U>): Parser<U> => {
  return ref(tokens => {
    const [t, rem, errs] = p.ref(tokens);
    return t.match({
      Ok: t => f(t, rem).ref(rem),
      Error: err => [error(Error.Parser({ message: fail }, pos(rem.start))), rem, [...errs, err]],
    });
  });
};

export const mapParserResult = <T, U>(p: Parser<T>, f: (r: ParserResult<T>) => ParserResult<U>): Parser<U> => {
  return ref(tokens => {
    return f(p.ref(tokens));
  });
};

export const invalid = mapParserResult(
  satisfy(token => token.variant === 'Invalid'),
  ([token, rem, errs]) => {
    return token.match({
      Ok: token => [
        ok(Expr.Error(Token.show(token), token.pos)),
        rem,
        [...errs, Error.Parser({
          message: Token.show(token),
        }, pos(rem.start))],
      ],
      Error: err => [error(err), rem, errs] as ParserResult<Expr>,
    });
  }
);

type MapP<Ts extends readonly any[]> =
  Ts extends [Parser<infer A>, ...infer As] ?
  [A, ...MapP<As>] :
  Ts extends [Parser<infer A>] ? [A] : [];

export const seq = <T extends readonly Parser<any>[]>(...parsers: T): Parser<[...MapP<T>]> => {
  return ref(tokens => {
    const originalTokens = tokens;
    const errors: Error[] = [];
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
    const errors: Error[] = [];

    for (const p of parsers) {
      const [t, rem, errs] = p.ref(tokens);
      errors.push(...errs);

      if (t.raw.type === 'ok') {
        return [t, rem, errors];
      }

      if (t.raw.data.variant === 'Parser' && t.raw.data.err.message !== fail) {
        return [t, tokens, errors];
      }
    }

    return [error(Error.Parser({ message: fail }, pos(tokens.start))), tokens, errors];
  });
};

export const many = <T>(p: Parser<T>): Parser<T[]> => {
  return ref(tokens => {
    const errors: Error[] = [];
    const ts: T[] = [];
    let isFirst = true;

    while (true) {
      const [t, rem, errs] = p.ref(tokens);
      errors.push(...errs);
      tokens = rem;

      if (t.raw.type === 'error') {
        if (isFirst && t.raw.data.variant === 'Parser' && t.raw.data.err.message !== fail) {
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
  combine: (left: L, right: R, pos: Position) => L
): Parser<L> => {
  return map(
    seq(left, many(right)),
    ([h, tl], pos) => tl.length === 0 ? h : tl.reduce((l, r) => combine(l, r, pos), h)
  );
};

export const chainLeft = <L, Op, R>(
  l: Parser<L>,
  op: Parser<Op>,
  r: Parser<R>,
  combine: (lhs: L, op: Op, rhs: R, pos: Position) => L
): Parser<L> => {
  return leftAssoc(l, seq(op, r), (lhs, [op, rhs], pos) => combine(lhs, op, rhs, pos));
};

export const sepBy = <S>(sep: Parser<S>) => <T>(p: Parser<T>, allowEmpty = false): Parser<T[]> => {
  const base = map(seq(p, many(seq(sep, p)), optional(sep)), ([h, tl]) => [h, ...tl.map(snd)]);

  if (allowEmpty) {
    return alt(base, map(yes(), () => []));
  }

  return base;
};

export const commas = sepBy(symbol(','));
export const semicolons = sepBy(symbol(';'));

export const not = <T>(p: Parser<T>, { consume } = { consume: true }): Parser<null> => {
  return ref(tokens => {
    const [t, rem] = p.ref(tokens);
    return t.match({
      Ok: () => [error(Error.Parser({ message: fail }, pos(tokens.start))), consume ? rem : tokens, []], // not.2
      Error: () => [ok(null), consume ? rem : tokens, []], // not.1
    });
  });
};

export const recover = (err: Error): Parser<null> => {
  return ref(tokens => {
    if (err.variant !== 'Parser' || err.err.recovery === undefined) {
      return [error(err), tokens, [err]]; // throw.1
    }

    const recoveredRem = err.err.recovery(tokens);
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
        const [_, rem2, errs2] = recover(Error.Parser({
          message,
          recovery: recovery ?? (e.variant === 'Parser' ? e.err.recovery : undefined),
        }, e.pos)).ref(tokens);

        return [error(Error.Parser({ message: fail }, pos(tokens.start))), rem2, errs2];
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
          return [
            error(Error.Parser({ message },
              pos(tokens.start))),
            rem,
            [...errs, Error.Parser({ message }, pos(tokens.start))],
          ];
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
      return [error(Error.Parser({ message: fail }, pos(tokens.start))), tokens, []];
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

        const err = Error.Parser({
          message: `Unexpected token: '${Token.show(lastToken)}'`,
        }, pos(rem.start));

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

export const effect = <T>(p: Parser<T>, action: (data: T, pos: Position, tokens: Slice<Token>) => void): Parser<T> => {
  return map(p, (data, pos, tokens) => {
    action(data, pos, tokens);
    return data;
  });
};

export const yes = (): Parser<undefined> => {
  return ref(tokens => {
    return [ok(undefined), tokens, []];
  });
};

export const parens = nestedBy('(', ')');
export const squareBrackets = nestedBy('[', ']');
export const curlyBrackets = nestedBy('{', '}');
