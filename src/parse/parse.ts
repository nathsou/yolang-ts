import { match, select } from 'ts-pattern';
import { Expr } from '../ast/sweet';
import { ref } from '../utils/misc';
import { error, ok, Result } from '../utils/result';
import { Slice } from '../utils/slice';
import { alt, chainLeft, map, parens, Parser, satisfyBy, seq, symbol } from './combinators';
import { Const, Token } from './token';

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

const expr: Parser<Expr> = ref(() => error('unset'));

const primary = alt(integer, bool, parens(expr));

const factor = primary;

const unary = alt(map(seq(unaryOp, factor), ([op, expr]) => Expr.unaryOp(op, expr)), factor);

const term = chainLeft(unary, multiplicativeOp, (a, op, b) => Expr.binaryOp(a, op, b));

const arith = chainLeft(term, additiveOp, (a, op, b) => Expr.binaryOp(a, op, b));

expr.ref = arith.ref;

export const parseExpr = (tokens: Slice<Token>): Result<Expr, string> => {
  return expr.ref(tokens).map(([expr]) => expr);
};