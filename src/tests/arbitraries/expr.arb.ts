import fc from 'fast-check';
import { Argument, Expr } from '../../ast/sweet';
import { Const } from '../../parse/token';
import { none } from '../../utils/maybe';
import { lowerIdent } from './common.arb';
import { patternArb } from './pattern.arb';

export const constIntExpr = fc.integer({ min: 1 }).map(n => Expr.Const(Const.i32(n)));
export const constBoolExpr = fc.boolean().map(b => Expr.Const(Const.bool(b)));
export const constUnitExpr = fc.constant(Expr.Const(Const.unit()));
export const varExpr = lowerIdent.map(Expr.Variable);

export const constExpr = fc.frequency(
  { arbitrary: constIntExpr, weight: 4 },
  { arbitrary: constBoolExpr, weight: 2 },
  { arbitrary: constUnitExpr, weight: 1 },
);

export const constExprWithoutInt = fc.frequency(
  { arbitrary: constBoolExpr, weight: 2 },
  { arbitrary: constUnitExpr, weight: 1 },
);

const closureArgument = (maxDepth: number): fc.Arbitrary<Argument> =>
  fc.tuple(
    patternArb(maxDepth),
    fc.frequency(
      { arbitrary: fc.constant(false), weight: 2 },
      { arbitrary: fc.constant(true), weight: 1 },
    ),
    fc.frequency(
      { arbitrary: fc.constant(none), weight: 3 },
      // TODO: add annotations when records types can be parsed 
      // { arbitrary: Arb.ty(maxDepth).map(some), weight: 1 },
    )
  ).map(([pattern, mutable, annotation]) => ({ pattern, mutable, annotation }));

export const exprArb = (maxDepth = 3) => fc.letrec(tie => ({
  primary: fc.frequency(
    { arbitrary: constExpr, weight: 3 },
    { arbitrary: varExpr, weight: 1 },
    { arbitrary: tie('expr').map(expr => Expr.Parenthesized(expr as Expr)), weight: 2 },
  ),
  primaryWithoutIntConst: fc.frequency(
    { arbitrary: constExprWithoutInt, weight: 3 },
    { arbitrary: varExpr, weight: 1 },
    { arbitrary: tie('expr').map(expr => Expr.Parenthesized(expr as Expr)), weight: 2 },
  ),
  tuple: fc.array(tie('expr'), { minLength: 2 }).map(elems => Expr.Tuple(elems as Expr[])),
  unaryOp: fc.tuple(
    fc.oneof(...['-', 'not'].map(fc.constant)),
    tie('primaryWithoutIntConst'),
  ).map(([op, expr]) => Expr.Call(Expr.Variable(op), [], [expr as Expr])),
  binaryOp: fc.tuple(
    tie('unaryOp'),
    fc.oneof(...['+', '-', '*', '/', 'mod', 'or', 'and', 'nand', 'nor', 'xor', 'xnor', '==', '!=', '<', '>', '<=', '>='].map(fc.constant)),
    tie('unaryOp')
  ).map(([lhs, op, rhs]) => Expr.Call(Expr.Variable(op), [], [lhs as Expr, rhs as Expr])),
  assignment: fc.tuple(
    tie('binaryOp'),
    fc.constant('='),
    tie('binaryOp')
  ).map(([lhs, _, rhs]) => Expr.Assignment(lhs as Expr, rhs as Expr)),
  closure: fc.tuple(
    fc.array(closureArgument(maxDepth)),
    tie('expr'),
  ).map(([args, body]) => Expr.Closure(args, body as Expr)),
  expr: fc.frequency({ maxDepth },
    { arbitrary: tie('primary'), weight: 2 },
    { arbitrary: tie('tuple'), weight: 1 },
    { arbitrary: tie('unaryOp'), weight: 1 },
    { arbitrary: tie('binaryOp'), weight: 2 },
    { arbitrary: tie('assignment'), weight: 1 },
    { arbitrary: tie('closure'), weight: 1 },
  ),
}));
