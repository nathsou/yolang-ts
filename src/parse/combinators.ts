import { firstOkBy } from "../utils/array";
import { Ref, ref, snd } from "../utils/misc";
import { error, ok, Result } from "../utils/result";
import { Slice } from "../utils/slice";
import { Keyword, Symbol, Token } from "./token";

type ParserError = string;

export type Parser<T> = Ref<(tokens: Slice<Token>) => Result<[T, Slice<Token>], ParserError>>;

const mapHead = <T, U>(
  slice: Slice<T>,
  f: (data: T) => Result<U, ParserError>
): Result<U, ParserError> => {
  return Slice.head(slice).match({
    Some: f,
    None: () => error('no more tokens'),
  });
};

const parser = <T>(f: (token: Token) => Result<T, ParserError>): Parser<T> => {
  return ref(tokens => mapHead(tokens, f).map(t => [t, Slice.tail(tokens)]));
};

export const satisfy = (pred: (t: Token) => boolean, name = () => pred.name): Parser<Token> => {
  return parser(token => {
    if (pred(token)) {
      return ok(token);
    } else {
      return error(`token ${Token.show(token)} does not satisfy '${name()}'`);
    }
  });
};

export const satisfyBy = <T>(f: (t: Token) => Result<T, ParserError>): Parser<T> => {
  return ref(tokens => Slice.head(tokens).match({
    Some: h => f(h).map(t => [t, Slice.tail(tokens)]),
    None: () => error('condition not satisfied in satisfyBy'),
  }));
};

export const token = (token: Token): Parser<Token> => {
  return satisfy(t => Token.eq(t, token), () => `token(${Token.show(token)})`)
};

export const symbol = (symb: Symbol) => token(Token.symbol(symb));
export const keyword = (keyword: Keyword) => token(Token.keyword(keyword));

export const map = <T, U>(p: Parser<T>, f: (t: T) => U): Parser<U> => {
  return ref(tokens => p.ref(tokens).map(([t, rem]) => [f(t), rem]));
};

const then = <A, B>(pa: Parser<A>, pb: Parser<B>): Parser<[A, B]> => {
  return ref(tokens => {
    return pa.ref(tokens).flatMap(([a, rem1]) =>
      pb.ref(rem1).map(([b, rem2]) => [[a, b], rem2])
    );
  });
};

type MapP<Ts extends readonly any[]> =
  Ts extends [Parser<infer A>, ...infer As] ?
  [A, ...MapP<As>] :
  Ts extends [Parser<infer A>] ? [A] : [];

export const seq = <T extends readonly Parser<any>[]>(...parsers: T): Parser<[...MapP<T>]> => {
  if (parsers.length === 0) {
    return ref(tokens => ok([[], tokens])) as Parser<[...MapP<T>]>;
  }

  const [p, ...ps] = parsers;
  return map(then(p, seq(...ps)), ([h, t]) => [h, ...t]) as Parser<[...MapP<T>]>;
};

export const alt = <T>(...parsers: Parser<T>[]): Parser<T> => {
  return ref(tokens => firstOkBy(
    parsers,
    p => p.ref(tokens),
    'no alternative matched the input'
  ));
};

export const oneOrMore = <T>(p: Parser<T>): Parser<T[]> => {
  return map(seq(p, many(p)), ([h, tl]) => [h, ...tl]);
};

export const many = <T>(p: Parser<T>): Parser<T[]> => {
  const aux = (tokens: Slice<Token>): [T[], Slice<Token>] => {
    if (Slice.isEmpty(tokens)) {
      return [[], tokens];
    }

    return p.ref(tokens).match({
      Ok: ([t, rem1]) => {
        const [ts, rem2] = aux(rem1);
        return [[t, ...ts], rem2];
      },
      Error: () => [[], tokens],
    });
  };

  return ref(tokens => ok(aux(tokens)));
};

export const leftAssoc = <A, B>(
  left: Parser<A>,
  right: Parser<B>,
  combine: (left: A, right: B) => A
): Parser<A> => {
  return map(
    seq(left, many(right)),
    ([h, tl]) => tl.length === 0 ? h : tl.reduce((acc, r) => combine(acc, r), h)
  );
};

export const chainLeft = <A, B>(
  p: Parser<A>,
  op: Parser<B>,
  combine: (lhs: A, op: B, rhs: A) => A
): Parser<A> => {
  return leftAssoc(p, seq(op, p), (lhs, [op, rhs]) => combine(lhs, op, rhs));
};

export const parens = <T>(p: Parser<T>): Parser<T> => {
  return map(seq(symbol('('), p, symbol(')')), ([, t,]) => t);
};

export const sepBy = <S>(sep: Parser<S>) => <T>(p: Parser<T>): Parser<T[]> => {
  return map(seq(p, many(seq(sep, p))), ([h, tl]) => [h, ...tl.map(snd)]);
};

export const commas = sepBy(symbol(','));