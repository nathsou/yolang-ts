import { DataType, match as matchVariant } from 'itsamatch';
import { Const } from '../parse/token';
import { joinWith } from '../utils/array';
import { Maybe } from '../utils/maybe';

// Sweet expressions are *sugared* representations
// of the structure of yolang source code.

export type UnaryOperator = '-' | '!';
export type BinaryOperator = '+' | '-' | '*' | '/' | '%' | '==' | '!=' | '<' | '>' | '<=' | '>=' | '&&' | '||';
export type CompoundAssignmentOperator = '+=' | '-=' | '*=' | '/=' | '%=' | '&&=' | '||=';

export type Expr = DataType<{
  Const: { value: Const },
  Variable: { name: string },
  Call: { lhs: Expr, args: Expr[] },
  BinaryOp: { lhs: Expr, op: BinaryOperator, rhs: Expr },
  UnaryOp: { op: UnaryOperator, expr: Expr },
  Error: { message: string },
  Closure: { args: { name: string, mutable: boolean }[], body: Expr },
  Block: { statements: Stmt[] },
  IfThenElse: { condition: Expr, then: Expr, else_: Maybe<Expr> },
  Assignment: { lhs: Expr, rhs: Expr },
  CompoundAssignment: { lhs: Expr, op: CompoundAssignmentOperator, rhs: Expr },
  ModuleAccess: { path: string[], member: string },
  Tuple: { elements: Expr[] },
}>;

export const Expr = {
  Const: (c: Const): Expr => ({ variant: 'Const', value: c }),
  Variable: (name: string): Expr => ({ variant: 'Variable', name }),
  Call: (lhs: Expr, args: Expr[]): Expr => ({ variant: 'Call', lhs, args }),
  BinaryOp: (lhs: Expr, op: BinaryOperator, rhs: Expr): Expr => ({ variant: 'BinaryOp', lhs, op, rhs }),
  UnaryOp: (op: UnaryOperator, expr: Expr): Expr => ({ variant: 'UnaryOp', op, expr }),
  Error: (message: string): Expr => ({ variant: 'Error', message }),
  Closure: (args: { name: string, mutable: boolean }[], body: Expr): Expr => ({ variant: 'Closure', args, body }),
  Block: (statements: Stmt[]): Expr => ({ variant: 'Block', statements }),
  IfThenElse: (condition: Expr, then: Expr, else_: Maybe<Expr>): Expr => ({ variant: 'IfThenElse', condition, then, else_ }),
  Assignment: (lhs: Expr, rhs: Expr): Expr => ({ variant: 'Assignment', lhs, rhs }),
  CompoundAssignment: (lhs: Expr, op: CompoundAssignmentOperator, rhs: Expr): Expr => ({ variant: 'CompoundAssignment', lhs, op, rhs }),
  ModuleAccess: (path: string[], member: string): Expr => ({ variant: 'ModuleAccess', path, member }),
  Tuple: (elements: Expr[]): Expr => ({ variant: 'Tuple', elements }),
  show: (expr: Expr): string => matchVariant(expr, {
    Const: ({ value: expr }) => Const.show(expr),
    Variable: ({ name }) => name,
    Call: ({ lhs, args }) => `${Expr.show(lhs)}(${joinWith(args, Expr.show, ', ')})`,
    UnaryOp: ({ op, expr }) => `${op}${Expr.show(expr)}`,
    BinaryOp: ({ lhs, op, rhs }) => `(${Expr.show(lhs)} ${op} ${Expr.show(rhs)})`,
    Error: ({ message }) => `<Error: ${message}>`,
    Closure: ({ args, body }) => `(${joinWith(args, ({ name, mutable }) => `${mutable ? 'mut ' : ''}${name}`, ', ')}) -> ${Expr.show(body)}`,
    Block: ({ statements }) => `{\n${joinWith(statements, s => '  ' + Stmt.show(s), '\n')}\n}`,
    IfThenElse: ({ condition, then, else_ }) => `if ${Expr.show(condition)} ${Expr.show(then)}${else_.map(e => ` else ${Expr.show(e)}`).orDefault('')}`,
    Assignment: ({ lhs, rhs }) => `${Expr.show(lhs)} = ${Expr.show(rhs)}`,
    CompoundAssignment: ({ lhs, op, rhs }) => `${Expr.show(lhs)} ${op} ${Expr.show(rhs)}`,
    ModuleAccess: ({ path, member }) => `${path.join('.')}.${member}`,
    Tuple: ({ elements }) => `(${joinWith(elements, Expr.show, ', ')})`,
  }),
};

export type Stmt = DataType<{
  Let: { name: string, expr: Expr, mutable: boolean },
  Expr: { expr: Expr },
  Error: { message: string },
}>;

export const Stmt = {
  Let: (name: string, expr: Expr, mutable: boolean): Stmt => ({ variant: 'Let', name, expr, mutable }),
  Expr: (expr: Expr): Stmt => ({ variant: 'Expr', expr }),
  Error: (message: string): Stmt => ({ variant: 'Error', message }),
  show: (stmt: Stmt): string => matchVariant(stmt, {
    Let: ({ name, expr, mutable }) => `${mutable ? 'mut' : 'let'} ${name} = ${Expr.show(expr)}`,
    Expr: ({ expr }) => Expr.show(expr),
    Error: ({ message }) => `<Error: ${message}>`,
  }),
};

export type Decl = DataType<{
  Function: { name: string, args: { name: string, mutable: boolean }[], body: Expr },
  Module: { name: string, decls: Decl[] },
  Error: { message: string },
}>;

export const Decl = {
  Function: (name: string, args: { name: string, mutable: boolean }[], body: Expr): Decl => ({ variant: 'Function', name, args, body }),
  Module: (name: string, decls: Decl[]): Decl => ({ variant: 'Module', name, decls }),
  Error: (message: string): Decl => ({ variant: 'Error', message }),
  show: (decl: Decl): string => matchVariant(decl, {
    Function: ({ name, args, body }) => `fn ${name}(${joinWith(args, ({ name, mutable }) => `${mutable ? 'mut ' : ''}${name}`, ', ')}) ${Expr.show(body)}`,
    Module: ({ name, decls }) => `module ${name} {\n${joinWith(decls, d => '  ' + Decl.show(d), '\n')}\n}`,
    Error: ({ message }) => `<Error: ${message}>`,
  }),
};

export type Prog = Decl[];