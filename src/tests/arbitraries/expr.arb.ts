import fc from 'fast-check';
import { BinaryOperator, CompoundAssignmentOperator, Expr, UnaryOperator } from '../../ast/sweet';
import { Const, Keyword } from '../../parse/token';
import { isAlpha, isAlphaNum, isLowerCaseLetter, isUpperCase } from '../../utils/strings';

const lowerAlpha = fc.char().filter(isLowerCaseLetter);
const upperAlpha = fc.char().filter(isUpperCase);
const alpha = fc.char().filter(isAlpha);
const alphaNum = fc.char().filter(isAlphaNum);

export const constU32Expr = fc.integer({ min: 0 }).map(n => Expr.Const(Const.u32(n)));
export const constBoolExpr = fc.boolean().map(b => Expr.Const(Const.bool(b)));
export const constUnitExpr = fc.constant(Expr.Const(Const.unit()));
export const varExpr = fc.tuple(lowerAlpha, fc.array(alphaNum, { minLength: 0 }))
  .map(([h, tl]) => h + tl.join(''))
  .filter(ident => !Keyword.is(ident))
  .map(Expr.Variable);

export const constExpr = fc.frequency(
  { arbitrary: constU32Expr, weight: 4 },
  { arbitrary: constBoolExpr, weight: 2 },
  { arbitrary: constUnitExpr, weight: 1 },
);

export const expr = (maxDepth = 3) => fc.letrec(tie => ({
  primary: fc.frequency(
    { arbitrary: constExpr, weight: 3 },
    { arbitrary: varExpr, weight: 1 },
    { arbitrary: tie('expr').map(expr => Expr.Parenthesized(expr as Expr)), weight: 2 },
  ),
  tuple: fc.array(tie('expr'), { minLength: 2 }).map(elems => Expr.Tuple(elems as Expr[])),
  unaryOp: fc.tuple(
    fc.oneof(...['-', '!'].map(fc.constant)),
    tie('primary'),
  ).map(([op, expr]) => Expr.UnaryOp(op as UnaryOperator, expr as Expr)),
  binaryOp: fc.tuple(
    tie('unaryOp'),
    fc.oneof(...['+', '-', '*', '/', '%', '==', '!=', '<', '>', '<=', '>='].map(fc.constant)),
    tie('unaryOp')
  ).map(([lhs, op, rhs]) => Expr.BinaryOp(lhs as Expr, op as BinaryOperator, rhs as Expr)),
  assignment: fc.tuple(
    tie('binaryOp'),
    fc.constant('='),
    tie('binaryOp')
  ).map(([lhs, _, rhs]) => Expr.Assignment(lhs as Expr, rhs as Expr)),
  compoundAssignment: fc.tuple(
    tie('binaryOp'),
    fc.oneof(...['+=', '-=', '*=', '/=', '%='].map(fc.constant)),
    tie('binaryOp')
  ).map(([lhs, op, rhs]) => Expr.CompoundAssignment(lhs as Expr, op as CompoundAssignmentOperator, rhs as Expr)),
  expr: fc.frequency({ maxDepth },
    { arbitrary: tie('primary'), weight: 3 },
    { arbitrary: tie('tuple'), weight: 1 },
    { arbitrary: tie('unaryOp'), weight: 1 },
    { arbitrary: tie('binaryOp'), weight: 2 },
    { arbitrary: tie('assignment'), weight: 2 },
    { arbitrary: tie('compoundAssignment'), weight: 1 },
  ),
}));