import { DataType, match as matchVariant } from 'itsamatch';
import { MonoTy, ParameterizedTy, TyVarId } from '../infer/types';
import { Const } from '../parse/token';
import { joinWith } from '../utils/array';
import { Maybe } from '../utils/maybe';
import { parenthesized } from '../utils/misc';

// Sweet expressions are *sugared* representations
// of the structure of yolang source code.

export type UnaryOperator = '-' | '!';
export type BinaryOperator = '+' | '-' | '*' | '/' | '%' | '==' | '!=' | '<' | '>' | '<=' | '>=' | '&&' | '||';
export type CompoundAssignmentOperator = '+=' | '-=' | '*=' | '/=' | '%=' | '&&=' | '||=';

export type Argument = { pattern: Pattern, mutable: boolean };

export type Expr = DataType<{
  Const: { value: Const },
  Variable: { name: string },
  Call: { lhs: Expr, args: Expr[] },
  BinaryOp: { lhs: Expr, op: BinaryOperator, rhs: Expr },
  UnaryOp: { op: UnaryOperator, expr: Expr },
  Error: { message: string },
  Closure: { args: Argument[], body: Expr },
  Block: { statements: Stmt[] },
  IfThenElse: { condition: Expr, then: Expr, else_: Maybe<Expr> },
  Assignment: { lhs: Expr, rhs: Expr },
  CompoundAssignment: { lhs: Expr, op: CompoundAssignmentOperator, rhs: Expr },
  ModuleAccess: { path: string[], member: string },
  FieldAccess: { lhs: Expr, field: string },
  Tuple: { elements: Expr[] },
  Match: { expr: Expr, cases: { pattern: Pattern, body: Expr }[] },
}>;

export const Expr = {
  Const: (c: Const): Expr => ({ variant: 'Const', value: c }),
  Variable: (name: string): Expr => ({ variant: 'Variable', name }),
  Call: (lhs: Expr, args: Expr[]): Expr => ({ variant: 'Call', lhs, args }),
  BinaryOp: (lhs: Expr, op: BinaryOperator, rhs: Expr): Expr => ({ variant: 'BinaryOp', lhs, op, rhs }),
  UnaryOp: (op: UnaryOperator, expr: Expr): Expr => ({ variant: 'UnaryOp', op, expr }),
  Error: (message: string): Expr => ({ variant: 'Error', message }),
  Closure: (args: Argument[], body: Expr): Expr => ({ variant: 'Closure', args, body }),
  Block: (statements: Stmt[]): Expr => ({ variant: 'Block', statements }),
  IfThenElse: (condition: Expr, then: Expr, else_: Maybe<Expr>): Expr => ({ variant: 'IfThenElse', condition, then, else_ }),
  Assignment: (lhs: Expr, rhs: Expr): Expr => ({ variant: 'Assignment', lhs, rhs }),
  CompoundAssignment: (lhs: Expr, op: CompoundAssignmentOperator, rhs: Expr): Expr => ({ variant: 'CompoundAssignment', lhs, op, rhs }),
  ModuleAccess: (path: string[], member: string): Expr => ({ variant: 'ModuleAccess', path, member }),
  FieldAccess: (lhs: Expr, field: string): Expr => ({ variant: 'FieldAccess', lhs, field }),
  Tuple: (elements: Expr[]): Expr => ({ variant: 'Tuple', elements }),
  Match: (expr: Expr, cases: { pattern: Pattern, body: Expr }[]): Expr => ({ variant: 'Match', expr, cases }),
  show: (expr: Expr): string => matchVariant(expr, {
    Const: ({ value: expr }) => Const.show(expr),
    Variable: ({ name }) => name,
    Call: ({ lhs, args }) => `${Expr.show(lhs)}(${joinWith(args, Expr.show, ', ')})`,
    UnaryOp: ({ op, expr }) => `${op}${Expr.show(expr)}`,
    BinaryOp: ({ lhs, op, rhs }) => `(${Expr.show(lhs)} ${op} ${Expr.show(rhs)})`,
    Error: ({ message }) => `<Error: ${message}>`,
    Closure: ({ args, body }) => `${parenthesized(joinWith(args, ({ pattern, mutable }) => `${mutable ? 'mut ' : ''}${Pattern.show(pattern)}`, ', '), args.length !== 1 || (args.length > 0 && args[0].pattern.variant === 'Tuple'))} -> ${Expr.show(body)}`,
    Block: ({ statements }) => `{\n${joinWith(statements, s => '  ' + Stmt.show(s), '\n')}\n}`,
    IfThenElse: ({ condition, then, else_ }) => `if ${Expr.show(condition)} ${Expr.show(then)}${else_.map(e => ` else ${Expr.show(e)}`).orDefault('')}`,
    Assignment: ({ lhs, rhs }) => `${Expr.show(lhs)} = ${Expr.show(rhs)}`,
    CompoundAssignment: ({ lhs, op, rhs }) => `${Expr.show(lhs)} ${op} ${Expr.show(rhs)}`,
    ModuleAccess: ({ path, member }) => `${path.join('.')}.${member}`,
    FieldAccess: ({ lhs, field }) => `${Expr.show(lhs)}.${field}`,
    Tuple: ({ elements }) => `(${joinWith(elements, Expr.show, ', ')})`,
    Match: ({ expr, cases }) => `match ${Expr.show(expr)} {\n${joinWith(cases, ({ pattern, body }) => `  ${Pattern.show(pattern)} => ${Expr.show(body)}\n`, '\n')}\n}`,
  }),
};

export type Pattern = DataType<{
  Const: { value: Const },
  Variable: { name: string },
  Tuple: { elements: Pattern[] },
  Any: {},
  Error: { message: string },
}>;

export const Pattern = {
  Const: (value: Const): Pattern => ({ variant: 'Const', value }),
  Variable: (name: string): Pattern => ({ variant: 'Variable', name }),
  Tuple: (elements: Pattern[]): Pattern => ({ variant: 'Tuple', elements }),
  Any: (): Pattern => ({ variant: 'Any' }),
  Error: (message: string): Pattern => ({ variant: 'Error', message }),
  show: (pattern: Pattern): string => matchVariant(pattern, {
    Const: ({ value }) => `${Const.show(value)}`,
    Variable: ({ name }) => `${name}`,
    Tuple: ({ elements }) => `(${elements.map(Pattern.show).join(', ')})`,
    Any: () => '_',
    Error: ({ message }) => `<Error: ${message}>`,
  }),
  isIrrefutable: (pattern: Pattern): boolean => matchVariant(pattern, {
    Const: () => false,
    Variable: () => true,
    Tuple: ({ elements }) => elements.every(Pattern.isIrrefutable),
    Any: (): boolean => true,
    Error: (): boolean => false,
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

type Field = { name: string, ty: ParameterizedTy };

export type Decl = DataType<{
  Function: { name: string, args: Argument[], body: Expr },
  Module: { name: string, decls: Decl[] },
  Struct: { name: string, typeParams: string[], fields: Field[] },
  Error: { message: string },
}>;

export const Decl = {
  Function: (name: string, args: Argument[], body: Expr): Decl => ({ variant: 'Function', name, args, body }),
  Module: (name: string, decls: Decl[]): Decl => ({ variant: 'Module', name, decls }),
  Struct: (name: string, typeParams: string[], fields: Field[]): Decl => ({ variant: 'Struct', name, typeParams, fields }),
  Error: (message: string): Decl => ({ variant: 'Error', message }),
  show: (decl: Decl): string => matchVariant(decl, {
    Function: ({ name, args, body }) => `fn ${name}(${joinWith(args, ({ pattern, mutable }) => `${mutable ? 'mut ' : ''}${Pattern.show(pattern)}`, ', ')}) ${Expr.show(body)}`,
    Module: ({ name, decls }) => `module ${name} {\n${joinWith(decls, d => '  ' + Decl.show(d), '\n')}\n}`,
    Struct: ({ name, typeParams, fields }) => {
      const params = typeParams.length > 0 ? `<${typeParams.join(', ')}>` : '';
      return `type ${name}${params} = {\n${joinWith(fields, ({ name, ty }) => `  ${name}: ${ParameterizedTy.show(ty)}`, `,\n`)}\n}`;
    },
    Error: ({ message }) => `<Error: ${message}>`,
  }),
};

export type Prog = Decl[];

export const Prog = {
  show: (prog: Prog): string => joinWith(prog, Decl.show, '\n\n'),
};