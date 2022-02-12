import { DataType, match as matchVariant, VariantOf } from 'itsamatch';
import { MonoTy, TypeParams } from '../infer/types';
import { Const } from '../parse/token';
import { joinWith } from '../utils/array';
import { Maybe } from '../utils/maybe';
import { id, noop, parenthesized } from '../utils/misc';

// Sweet expressions are *sugared* representations
// of the structure of yolang source code.

export type UnaryOperator = '-' | '!';
export type BinaryOperator = '+' | '-' | '*' | '/' | '%' | '==' | '!=' | '<' | '>' | '<=' | '>=' | '&&' | '||';
export type CompoundAssignmentOperator = '+=' | '-=' | '*=' | '/=' | '%=' | '&&=' | '||=';

export type Argument = { pattern: Pattern, mutable: boolean, annotation: Maybe<MonoTy> };

export const Argument = {
  show: ({ pattern, mutable, annotation }: Argument) => {
    return `${mutable ? 'mut ' : ''}${Pattern.show(pattern)}${annotation.mapWithDefault(t => `: ${MonoTy.show(t)}`, '')}`;
  },
  asMonoTy: ({ annotation }: Argument): MonoTy => {
    return annotation.orDefault(MonoTy.fresh);
  },
};

export const ArgumentList = {
  show: (args: Argument[]) => parenthesized(
    joinWith(args, Argument.show, ', '),
    args.length !== 1 || true || (
      args[0].mutable ||
      args[0].annotation.isSome() ||
      args[0].pattern.variant === 'Tuple'
    ),
  ),
};

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
  MethodCall: { receiver: Expr, method: string, args: Expr[] },
  ModuleAccess: { path: string[], member: string },
  FieldAccess: { lhs: Expr, field: string },
  Tuple: { elements: Expr[] },
  Match: { expr: Expr, annotation: Maybe<MonoTy>, cases: { pattern: Pattern, annotation: Maybe<MonoTy>, body: Expr }[] },
  Parenthesized: { expr: Expr },
  NamedRecord: { path: string[], name: string, typeParams: MonoTy[], fields: { name: string, value: Expr }[] },
  TupleIndexing: { lhs: Expr, index: number },
  LetIn: { pattern: Pattern, annotation: Maybe<MonoTy>, value: Expr, body: Expr },
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
  MethodCall: (receiver: Expr, method: string, args: Expr[]): Expr => ({ variant: 'MethodCall', receiver, method, args }),
  ModuleAccess: (path: string[], member: string): Expr => ({ variant: 'ModuleAccess', path, member }),
  FieldAccess: (lhs: Expr, field: string): Expr => ({ variant: 'FieldAccess', lhs, field }),
  Tuple: (elements: Expr[]): Expr => ({ variant: 'Tuple', elements }),
  Match: (expr: Expr, annotation: Maybe<MonoTy>, cases: { pattern: Pattern, annotation: Maybe<MonoTy>, body: Expr }[]): Expr => ({ variant: 'Match', annotation, expr, cases }),
  Parenthesized: (expr: Expr): Expr => ({ variant: 'Parenthesized', expr }),
  NamedRecord: (path: string[], name: string, typeParams: MonoTy[], fields: { name: string, value: Expr }[]): Expr => ({ variant: 'NamedRecord', path, name, typeParams, fields }),
  TupleIndexing: (lhs: Expr, index: number): Expr => ({ variant: 'TupleIndexing', lhs, index }),
  LetIn: (pattern: Pattern, annotation: Maybe<MonoTy>, value: Expr, body: Expr): Expr => ({ variant: 'LetIn', pattern, annotation, value, body }),
  show: (expr: Expr): string => matchVariant(expr, {
    Const: ({ value: expr }) => Const.show(expr),
    Variable: ({ name }) => name,
    Call: ({ lhs, args }) => `${Expr.show(lhs)}(${joinWith(args, Expr.show, ', ')})`,
    UnaryOp: ({ op, expr }) => `${op}${Expr.show(expr)}`,
    BinaryOp: ({ lhs, op, rhs }) => `${Expr.show(lhs)} ${op} ${Expr.show(rhs)}`,
    Error: ({ message }) => `<Error: ${message}>`,
    Closure: ({ args, body }) => `${ArgumentList.show(args)} -> ${Expr.show(body)}`,
    Block: ({ statements }) => `{\n${joinWith(statements, s => '  ' + Stmt.show(s), '\n')}\n}`,
    IfThenElse: ({ condition, then, else_ }) => `if ${Expr.show(condition)} ${Expr.show(then)}${else_.map(e => ` else ${Expr.show(e)}`).orDefault('')}`,
    Assignment: ({ lhs, rhs }) => `${Expr.show(lhs)} = ${Expr.show(rhs)}`,
    CompoundAssignment: ({ lhs, op, rhs }) => `${Expr.show(lhs)} ${op} ${Expr.show(rhs)}`,
    MethodCall: ({ receiver, method, args }) => `${Expr.show(receiver)}.${method}(${joinWith(args, Expr.show, ', ')})`,
    ModuleAccess: ({ path, member }) => `${path.join('.')}.${member}`,
    FieldAccess: ({ lhs, field }) => `${Expr.show(lhs)}.${field}`,
    Tuple: ({ elements }) => `(${joinWith(elements, Expr.show, ', ')})`,
    Match: ({ expr, cases }) => `match ${Expr.show(expr)} {\n${joinWith(cases, ({ pattern, body }) => `  ${Pattern.show(pattern)} => ${Expr.show(body)}\n`, '\n')}\n}`,
    Parenthesized: ({ expr }) => `(${Expr.show(expr)})`,
    NamedRecord: ({ path, name, typeParams, fields }) => `${path.join('.')}${path.length > 0 ? '.' : ''}${name} <${joinWith(typeParams, MonoTy.show, ', ')}> {\n${joinWith(fields, ({ name, value }) => `${name}: ${Expr.show(value)}`, ', ')}\n}`,
    TupleIndexing: ({ lhs, index }) => `${Expr.show(lhs)}.${index}`,
    LetIn: ({ pattern, value, body }) => `let ${Pattern.show(pattern)} = ${Expr.show(value)} in ${Expr.show(body)}`,
  }),
  rewrite: (expr: Expr, f: (expr: Expr) => Expr): Expr => {
    const go = (e: Expr) => Expr.rewrite(e, f);
    return f(matchVariant(expr, {
      Const: ({ value: expr }) => Expr.Const(expr),
      Variable: ({ name }) => Expr.Variable(name),
      Call: ({ lhs, args }) => Expr.Call(f(lhs), args.map(go)),
      UnaryOp: ({ op, expr }) => Expr.UnaryOp(op, go(expr)),
      BinaryOp: ({ lhs, op, rhs }) => Expr.BinaryOp(go(lhs), op, go(rhs)),
      Error: ({ message }) => Expr.Error(message),
      Closure: ({ args, body }) => Expr.Closure(args, go(body)),
      Block: ({ statements: stmts }) => Expr.Block(stmts.map(s => Stmt.rewrite(s, f))),
      IfThenElse: ({ condition, then, else_ }) => Expr.IfThenElse(go(condition), go(then), else_.map(f)),
      Assignment: ({ lhs, rhs }) => Expr.Assignment(go(lhs), go(rhs)),
      CompoundAssignment: ({ lhs, op, rhs }) => Expr.CompoundAssignment(go(lhs), op, go(rhs)),
      MethodCall: ({ receiver, method, args }) => Expr.MethodCall(go(receiver), method, args.map(go)),
      ModuleAccess: ({ path, member }) => Expr.ModuleAccess(path, member),
      FieldAccess: ({ lhs, field }) => Expr.FieldAccess(go(lhs), field),
      Tuple: ({ elements }) => Expr.Tuple(elements.map(go)),
      Match: ({ expr, annotation, cases }) => Expr.Match(go(expr), annotation, cases.map(({ pattern, annotation, body }) => ({ pattern, annotation, body: go(body) }))),
      Parenthesized: ({ expr }) => Expr.Parenthesized(go(expr)),
      NamedRecord: ({ path, name, typeParams, fields }) => Expr.NamedRecord(path, name, typeParams, fields.map(({ name, value }) => ({ name, value: go(value) }))),
      TupleIndexing: ({ lhs, index }) => Expr.TupleIndexing(go(lhs), index),
      LetIn: ({ pattern, annotation, value, body }) => Expr.LetIn(pattern, annotation, go(value), go(body)),
    }))
  },
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
    Tuple: ({ elements }) => `(${joinWith(elements, Pattern.show)})`,
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
  Let: {
    name: string,
    expr: Expr,
    mutable: boolean,
    annotation: Maybe<MonoTy>
  },
  Expr: { expr: Expr },
  Error: { message: string },
}>;

export const Stmt = {
  Let: (name: string, expr: Expr, mutable: boolean, annotation: Maybe<MonoTy>): Stmt => ({ variant: 'Let', name, expr, mutable, annotation }),
  Expr: (expr: Expr): Stmt => ({ variant: 'Expr', expr }),
  Error: (message: string): Stmt => ({ variant: 'Error', message }),
  show: (stmt: Stmt): string => matchVariant(stmt, {
    Let: ({ name, expr, mutable, annotation }) => `${mutable ? 'mut' : 'let'} ${name}${annotation.mapWithDefault(ty => ': ' + MonoTy.show(ty), '')} = ${Expr.show(expr)}`,
    Expr: ({ expr }) => Expr.show(expr),
    Error: ({ message }) => `<Error: ${message}>`,
  }),
  rewrite: (stmt: Stmt, f: (expr: Expr) => Expr): Stmt => matchVariant(stmt, {
    Let: ({ name, expr, mutable, annotation }) => Stmt.Let(name, Expr.rewrite(expr, f), mutable, annotation),
    Expr: ({ expr }) => Stmt.Expr(Expr.rewrite(expr, f)),
    Error: ({ message }) => Stmt.Error(message),
  }),
};

export type Decl = DataType<{
  Function: { name: string, typeParams: TypeParams, args: Argument[], body: Expr },
  Module: { name: string, decls: Decl[] },
  TypeAlias: { name: string, typeParams: TypeParams, alias: MonoTy },
  InherentImpl: { ty: MonoTy, typeParams: TypeParams, decls: Decl[] },
  Trait: { name: string, typeParams: TypeParams, methods: MethodSig[] },
  TraitImpl: { trait: { path: string[], name: string, args: MonoTy[] }, typeParams: TypeParams, implementee: MonoTy, methods: Decl[] },
  Error: { message: string },
}>;

export const Decl = {
  Function: (name: string, typeParams: TypeParams, args: Argument[], body: Expr): VariantOf<Decl, 'Function'> => ({ variant: 'Function', name, typeParams, args, body }),
  Module: (name: string, decls: Decl[]): Decl => ({ variant: 'Module', name, decls }),
  TypeAlias: (name: string, typeParams: TypeParams, alias: MonoTy): Decl => ({ variant: 'TypeAlias', name, typeParams, alias }),
  Impl: (ty: MonoTy, typeParams: TypeParams, decls: Decl[]): Decl => ({ variant: 'InherentImpl', ty, typeParams, decls }),
  Trait: (name: string, typeParams: TypeParams, methods: MethodSig[]): Decl => ({ variant: 'Trait', name, typeParams, methods }),
  TraitImpl: (trait: { path: string[], name: string, args: MonoTy[] }, typeParams: TypeParams, implementee: MonoTy, methods: Decl[]): Decl => ({ variant: 'TraitImpl', trait, typeParams, implementee, methods }),
  Error: (message: string): Decl => ({ variant: 'Error', message }),
  show: (decl: Decl): string => matchVariant(decl, {
    Function: ({ name, typeParams, args, body }) => `fn ${name}${TypeParams.show(typeParams)}(${joinWith(args, Argument.show, ', ')}) ${Expr.show(body)}`,
    Module: ({ name, decls }) => `module ${name} {\n${joinWith(decls, d => '  ' + Decl.show(d), '\n')}\n}`,
    TypeAlias: ({ name, typeParams, alias }) => `type ${name}${TypeParams.show(typeParams)} = ${MonoTy.show(alias)}`,
    InherentImpl: ({ ty, typeParams, decls }) => `impl${TypeParams.show(typeParams)} ${MonoTy.show(ty)} {\n${joinWith(decls, d => '  ' + Decl.show(d), '\n')}\n}`,
    Trait: ({ name, typeParams, methods }) => `trait ${name}${TypeParams.show(typeParams)} {\n${joinWith(methods, m => '  ' + MethodSig.show(m), '\n')}\n}`,
    TraitImpl: ({ trait, typeParams, implementee, methods }) => `impl ${[...trait.path, trait.name].join('.')}${TypeParams.show(typeParams)} for ${MonoTy.show(implementee)} {\n${joinWith(methods, m => '  ' + Decl.show(m), '\n')}\n}`,
    Error: ({ message }) => `<Error: ${message}> `,
  }),
  rewrite: (decl: Decl, rfs: RewriteFuncs): Decl => {
    const { rewriteExpr: f = id, rewriteDecl: g = id } = rfs;
    return matchVariant(decl, {
      Function: ({ name, typeParams, args, body }) => g(Decl.Function(name, typeParams, args, Expr.rewrite(body, f))),
      Module: ({ name, decls }) => g(Decl.Module(name, decls.map(d => Decl.rewrite(d, rfs)))),
      TypeAlias: ({ name, typeParams, alias }) => g(Decl.TypeAlias(name, typeParams, alias)),
      InherentImpl: ({ ty, typeParams, decls }) => g(Decl.Impl(ty, typeParams, decls.map(d => Decl.rewrite(d, rfs)))),
      Trait: ({ name, typeParams, methods }) => g(Decl.Trait(name, typeParams, methods)),
      TraitImpl: ({ trait, typeParams, implementee, methods }) => g(Decl.TraitImpl(trait, typeParams, implementee, methods.map(m => Decl.rewrite(m, rfs)))),
      Error: ({ message }) => g(Decl.Error(message)),
    });
  },
  traverse: (decl: Decl, { traverseExpr = noop, traverseDecl = noop }: TraverseFuncs): Decl => Decl.rewrite(decl, {
    rewriteExpr: (expr: Expr): Expr => { traverseExpr(expr); return expr; },
    rewriteDecl: (decl: Decl): Decl => { traverseDecl(decl); return decl; },
  }),
};

export type MethodSig = {
  name: string,
  typeParams: TypeParams,
  args: Argument[],
  ret: MonoTy,
};

export const MethodSig = {
  make: (name: string, typeParams: TypeParams, args: Argument[], ret: MonoTy): MethodSig => ({ name, typeParams, args, ret }),
  show: (method: MethodSig): string => `fn ${method.name}${TypeParams.show(method.typeParams)}(${joinWith(method.args, Argument.show, ', ')}) -> ${MonoTy.show(method.ret)}`,
  asMonoTy: (method: MethodSig): MonoTy => MonoTy.Fun(method.args.map(Argument.asMonoTy), method.ret),
};

export type Prog = Decl[];

type RewriteFuncs = {
  rewriteExpr?: (expr: Expr) => Expr,
  rewriteDecl?: (decl: Decl) => Decl,
};

type TraverseFuncs = {
  traverseExpr?: (expr: Expr) => void,
  traverseDecl?: (decl: Decl) => void,
};

export const Prog = {
  show: (prog: Prog): string => joinWith(prog, Decl.show, '\n\n'),
  rewrite: (prog: Prog, rfs: RewriteFuncs): Prog => prog.map(d => Decl.rewrite(d, rfs)),
  traverse: (prog: Prog, rfs: TraverseFuncs): Prog => prog.map(d => Decl.traverse(d, rfs)),
};