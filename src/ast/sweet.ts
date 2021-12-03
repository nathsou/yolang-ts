import { DataType } from 'itsamatch';
import { Const } from '../parse/token';

// Sweet expressions are *unsugared* representations
// of the structure of yolang source code.

export type UnaryOperator = '-' | '!';
export type BinaryOperator = '+' | '-' | '*' | '/' | '%';

export type UnaryOp = { op: UnaryOperator, expr: Expr };
export type BinOp = { lhs: Expr, op: BinaryOperator, rhs: Expr };

export type Expr = DataType<{
  Const: { expr: Const },
  BinaryOp: { expr: BinOp },
  UnaryOp: { expr: UnaryOp },
}>;

export const Expr = {
  const: (c: Const): Expr => ({ variant: 'Const', expr: c }),
  binaryOp: (lhs: Expr, op: BinaryOperator, rhs: Expr): Expr => ({ variant: 'BinaryOp', expr: { lhs, op, rhs } }),
  unaryOp: (op: UnaryOperator, expr: Expr): Expr => ({ variant: 'UnaryOp', expr: { op, expr } }),
};