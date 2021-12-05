import { match, select } from 'ts-pattern';
import { Expr } from '../ast/sweet';
import { Maybe, none, some } from '../utils/maybe';
import { error, ok } from '../utils/result';
import { Slice } from '../utils/slice';
import { alt, chainLeft, consumeAll, expect, initParser, map, mapParserResult, Parser, ParserError, ParserResult, satisfy, satisfyBy, seq, symbol, uninitialized } from './combinators';
import { RecoveryStrategy } from './recovery';
import { Const, Symbol, Token } from './token';

const expr = uninitialized<Expr>();

export const nestedBy = (left: Symbol, right: Symbol) => <T>(p: Parser<T>): Parser<Maybe<T>> => {
  return map(
    seq(
      symbol(left),
      expect(p, `Expected expression after '${left}'`),
      expect(symbol(right), `Expected closing '${right}'`, RecoveryStrategy.skipNested(left, right))
    ),
    ([, t,]) => t
  );
};

export const parens = (p: Parser<Expr>) => map(nestedBy('(', ')')(p), t => {
  return t.match({
    Some: t => t,
    None: () => Expr.error('Expected expression'),
  });
});

export const squareBrackets = nestedBy('[', ']');
export const curlyBrackets = nestedBy('{', '}');

const integer = satisfyBy<Expr>(token =>
  match(token)
    .with({
      variant: 'Const',
      value: {
        variant: 'u32',
        value: select()
      }
    },
      n => some(Expr.const(Const.u32(n)))
    )
    .otherwise(() => none)
);

const bool = satisfyBy<Expr>(token =>
  match(token)
    .with({
      variant: 'Const',
      value: {
        variant: 'bool',
        value: select()
      }
    },
      b => some(Expr.const(Const.bool(b)))
    )
    .otherwise(() => none)
);

const invalid = mapParserResult(
  satisfy(token => token.variant === 'Invalid'),
  ([token, rem, errs]) => token.match({
    Ok: token => [
      ok(Expr.error(Token.show(token))),
      rem,
      [...errs, {
        message: Token.show(token),
        pos: rem.start,
      }],
    ],
    Error: err => [error(err), rem, errs] as ParserResult<Expr>,
  })
);

const unexpected = mapParserResult(
  satisfy(() => true),
  ([token, rem, errs]) => {
    const msg = `Unexpected token: '${Token.show(token.unwrap())}'`;
    return [
      token.map(() => Expr.error(msg)),
      rem,
      [...errs, {
        message: msg,
        pos: rem.start,
      }],
    ];
  }
);

const unaryOp = alt(
  map(symbol('-'), () => '-' as const),
  map(symbol('!'), () => '!' as const),
);

const multiplicativeOp = alt(
  map(symbol('*'), () => '*' as const),
  map(symbol('/'), () => '/' as const),
  map(symbol('%'), () => '%' as const),
);

const additiveOp = alt(
  map(symbol('+'), () => '+' as const),
  map(symbol('-'), () => '-' as const),
);

const primary = alt(integer, bool, parens(expr), invalid, unexpected);

const factor = primary;

const unary = alt(
  map(seq(unaryOp, factor), ([op, expr]) => Expr.unaryOp(op, expr)),
  factor
);

const multiplicative = chainLeft(
  unary,
  multiplicativeOp,
  expect(unary, 'Expected expression after multiplicative operator'),
  (a, op, b) => b.mapWithDefault(
    b => Expr.binaryOp(a, op, b),
    Expr.error(`Expected expression after '${op}' operator`)
  )
);

const additive = chainLeft(
  multiplicative,
  additiveOp,
  expect(multiplicative, 'Expected expression after additive operator'),
  (a, op, b) => b.mapWithDefault(
    b => Expr.binaryOp(a, op, b),
    Expr.error(`Expected expression after '${op}' operator`)
  )
);

initParser(expr, additive);

export const parseExpr = (tokens: Slice<Token>): [Expr, ParserError[]] => {
  const [res, _, errs] = consumeAll(expr).ref(tokens);

  return res.match({
    Ok: e => [e, errs],
    Error: e => [Expr.error(e.message), [...errs, e]],
  });
};