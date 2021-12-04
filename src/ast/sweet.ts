import { DataType, match } from 'itsamatch';
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
  Error: { message: string },
}>;

export const Expr = {
  const: (c: Const): Expr => ({ variant: 'Const', expr: c }),
  binaryOp: (lhs: Expr, op: BinaryOperator, rhs: Expr): Expr => ({ variant: 'BinaryOp', expr: { lhs, op, rhs } }),
  unaryOp: (op: UnaryOperator, expr: Expr): Expr => ({ variant: 'UnaryOp', expr: { op, expr } }),
  error: (message: string): Expr => ({ variant: 'Error', message }),
  show: (expr: Expr): string => match(expr, {
    Const: ({ expr }) => Const.show(expr),
    UnaryOp: ({ expr: { op, expr } }) => `${op}${Expr.show(expr)}`,
    BinaryOp: ({ expr: { lhs, op, rhs } }) => `(${Expr.show(lhs)} ${op} ${Expr.show(rhs)})`,
    Error: ({ message }) => `<Error: ${message}>`,
  }),
};