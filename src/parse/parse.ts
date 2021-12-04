import { match, select } from 'ts-pattern';
import { Expr } from '../ast/sweet';
import { Maybe } from '../utils/maybe';
import { ref } from '../utils/misc';
import { error, ok } from '../utils/result';
import { Slice } from '../utils/slice';
import { alt, chainLeft, consumeAll, expect, map, Parser, ParserError, satisfyBy, seq, symbol } from './combinators';
import { RecoveryStrategy } from './recovery';
import { Const, Symbol, Token } from './token';

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
      n => ok<Expr, string>(Expr.const(Const.u32(n)))
    )
    .otherwise(() => error('token is not an integer'))
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
      b => ok<Expr, string>(Expr.const(Const.bool(b)))
    )
    .otherwise(() => error('token is not a bool'))
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

const expr: Parser<Expr> = ref(tokens => [error({ message: 'unset', pos: tokens.start }), tokens, []]);

const primary = alt(integer, bool, parens(expr));

const factor = primary;

const unary = alt(map(seq(unaryOp, factor), ([op, expr]) => Expr.unaryOp(op, expr)), factor);

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

expr.ref = additive.ref;

export const parseExpr = (tokens: Slice<Token>): [Expr, ParserError[]] => {
  const [res, _, errs] = consumeAll(expr).ref(tokens);

  return res.match({
    Ok: e => [e, errs],
    Error: e => [Expr.error(e.message), [...errs, e]],
  });
};