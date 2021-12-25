import { DataType, match as matchVariant, VariantOf } from "itsamatch";
import { Impl } from "../infer/impls";
import { MonoTy, ParameterizedTy, PolyTy, TypeParams } from "../infer/types";
import { Const } from "../parse/token";
import { Maybe, none } from "../utils/maybe";
import { Argument as SweetArgument, BinaryOperator, CompoundAssignmentOperator, Decl as SweetDecl, Expr as SweetExpr, Pattern as SweetPattern, Prog as SweetProg, Stmt as SweetStmt, UnaryOperator } from "./sweet";

// Bitter expressions are *unsugared* representations
// of the structure of yolang source code
// with attached type information and lexically resolved identifier references

type Name = {
  readonly original: string,
  renaming: string,
  ty: MonoTy,
  readonly mutable: boolean,
};

const Name = {
  fresh: (name: string, mutable: boolean): Name => ({
    original: name,
    renaming: name,
    ty: MonoTy.fresh(),
    mutable,
  }),
};

type NameEnv = Record<string, Name>;

const NameEnv = {
  make: (): NameEnv => ({}),
  clone: (env: NameEnv): NameEnv => ({ ...env }),
  declare: (env: NameEnv, name: string, mutable: boolean): Name => {
    const fresh = Name.fresh(name, mutable);
    env[name] = fresh;
    return fresh;
  },
  resolve: (env: NameEnv, name: string): Name => {
    if (name in env) {
      return env[name];
    }

    // TODO: return a dummy value
    // so that this error gets handled in the inferencer
    throw new Error(`Name '${name}' not found`);
  },
};

type WithSweetRefAndType<T> = {
  [K in keyof T]: T[K] & { sweet: SweetExpr, ty: MonoTy }
};

export type Pattern = DataType<{
  Const: { value: Const },
  Variable: { name: Name },
  Tuple: { elements: Pattern[] },
  Any: {},
  Error: { message: string },
}>;

export const Pattern = {
  Const: (value: Const): Pattern => ({ variant: 'Const', value }),
  Variable: (name: Name): Pattern => ({ variant: 'Variable', name }),
  Tuple: (elements: Pattern[]): Pattern => ({ variant: 'Tuple', elements }),
  Any: (): Pattern => ({ variant: 'Any' }),
  Error: (message: string): Pattern => ({ variant: 'Error', message }),
  fromSweet: (sweet: SweetPattern, nameEnv: NameEnv): Pattern => matchVariant(sweet, {
    Const: ({ value }): Pattern => Pattern.Const(value),
    Variable: ({ name }): Pattern => Pattern.Variable(NameEnv.declare(nameEnv, name, false)),
    Tuple: ({ elements }): Pattern => Pattern.Tuple(elements.map(e => Pattern.fromSweet(e, nameEnv))),
    Any: (): Pattern => Pattern.Any(),
    Error: ({ message }): Pattern => Pattern.Error(message),
  }),
  type: (pattern: Pattern): MonoTy => matchVariant(pattern, {
    Const: ({ value }) => Const.type(value),
    Variable: ({ name }) => name.ty,
    Tuple: ({ elements }) => MonoTy.tuple(elements.map(Pattern.type)),
    Any: (): MonoTy => MonoTy.fresh(),
    Error: (): MonoTy => MonoTy.fresh(),
  }),
  vars: (pattern: Pattern): Name[] => matchVariant(pattern, {
    Const: () => [],
    Variable: ({ name }) => [name],
    Tuple: ({ elements }) => elements.flatMap(Pattern.vars),
    Any: (): Name[] => [],
    Error: (): Name[] => [],
  }),
};

type Argument = { name: Name, mutable: boolean, annotation: Maybe<MonoTy> };

export type Expr = DataType<WithSweetRefAndType<{
  Const: { value: Const },
  Variable: { name: Name },
  Call: { lhs: Expr, args: Expr[] },
  BinaryOp: { lhs: Expr, op: BinaryOperator, rhs: Expr },
  UnaryOp: { op: UnaryOperator, expr: Expr },
  Error: { message: string },
  Closure: { args: Argument[], body: Expr },
  Block: { statements: Stmt[] },
  IfThenElse: { condition: Expr, then: Expr, else_: Maybe<Expr> },
  Assignment: { lhs: Expr, rhs: Expr },
  MethodCall: { receiver: Expr, method: string, args: Expr[], impl: Maybe<Impl> },
  ModuleAccess: { path: string[], member: string },
  FieldAccess: { lhs: Expr, field: string },
  Tuple: { elements: Expr[] },
  Match: { expr: Expr, cases: { pattern: Pattern, body: Expr }[] },
  NamedRecord: { name: string, typeParams: ParameterizedTy[], fields: { name: string, value: Expr }[] },
}>>;

const typed = <T extends {}>(obj: T, sweet: SweetExpr): T & { ty: MonoTy, sweet: SweetExpr } => ({
  ...obj,
  sweet,
  ty: MonoTy.fresh(),
});

export const Expr = {
  Const: (c: Const, sweet: SweetExpr): Expr => ({ variant: 'Const', value: c, sweet, ty: Const.type(c) }),
  Variable: (name: Name, sweet: SweetExpr): Expr => typed({ variant: 'Variable', name }, sweet),
  Call: (lhs: Expr, args: Expr[], sweet: SweetExpr): Expr => typed({ variant: 'Call', lhs, args }, sweet),
  BinaryOp: (lhs: Expr, op: BinaryOperator, rhs: Expr, sweet: SweetExpr): Expr => typed({ variant: 'BinaryOp', lhs, op, rhs }, sweet),
  UnaryOp: (op: UnaryOperator, expr: Expr, sweet: SweetExpr): Expr => typed({ variant: 'UnaryOp', op, expr }, sweet),
  Error: (message: string, sweet: SweetExpr): Expr => ({ variant: 'Error', message, sweet, ty: MonoTy.unit() }),
  Closure: (args: Argument[], body: Expr, sweet: SweetExpr): Expr => typed({ variant: 'Closure', args, body }, sweet),
  Block: (statements: Stmt[], sweet: SweetExpr): Expr => typed({ variant: 'Block', statements }, sweet),
  IfThenElse: (condition: Expr, then: Expr, else_: Maybe<Expr>, sweet: SweetExpr): Expr => typed({ variant: 'IfThenElse', condition, then, else_ }, sweet),
  Assignment: (lhs: Expr, rhs: Expr, sweet: SweetExpr): Expr => typed({ variant: 'Assignment', lhs, rhs }, sweet),
  MethodCall: (receiver: Expr, method: string, args: Expr[], sweet: SweetExpr): Expr => typed({ variant: 'MethodCall', receiver, method, args, impl: none }, sweet),
  ModuleAccess: (path: string[], member: string, sweet: SweetExpr): Expr => typed({ variant: 'ModuleAccess', path, member }, sweet),
  FieldAccess: (lhs: Expr, field: string, sweet: SweetExpr): Expr => typed({ variant: 'FieldAccess', lhs, field }, sweet),
  Tuple: (elements: Expr[], sweet: SweetExpr): Expr => typed({ variant: 'Tuple', elements }, sweet),
  Match: (expr: Expr, cases: { pattern: Pattern, body: Expr }[], sweet: SweetExpr): Expr => typed({ variant: 'Match', expr, cases }, sweet),
  NamedRecord: (name: string, typeParams: ParameterizedTy[], fields: { name: string, value: Expr }[], sweet: SweetExpr): Expr => typed({ variant: 'NamedRecord', name, typeParams, fields }, sweet),
  fromSweet: (sweet: SweetExpr, nameEnv: NameEnv, errors: BitterConversionError[]): Expr => {
    const go = (expr: SweetExpr, env = nameEnv) => Expr.fromSweet(expr, env, errors);

    return matchVariant(sweet, {
      Const: ({ value }) => Expr.Const(value, sweet),
      Variable: ({ name }) => Expr.Variable(NameEnv.resolve(nameEnv, name), sweet),
      Call: ({ lhs, args }) => Expr.Call(go(lhs), args.map(arg => go(arg)), sweet),
      BinaryOp: ({ lhs, op, rhs }) => Expr.BinaryOp(go(lhs), op, go(rhs), sweet),
      UnaryOp: ({ op, expr }) => Expr.UnaryOp(op, go(expr), sweet),
      Error: ({ message }) => Expr.Error(message, sweet),
      Closure: ({ args, body }) => {
        const withoutPatterns = removeFuncArgsPatternMatching(args, body, nameEnv, errors);
        return Expr.Closure(
          withoutPatterns.args,
          withoutPatterns.body,
          sweet
        );
      },
      Block: ({ statements }) => {
        const newNameEnv = NameEnv.clone(nameEnv);
        return Expr.Block(statements.map(s => Stmt.fromSweet(s, newNameEnv, errors)), sweet);
      },
      IfThenElse: ({ condition, then, else_ }) => Expr.IfThenElse(go(condition), go(then), else_.map(go), sweet),
      Assignment: ({ lhs, rhs }) => Expr.Assignment(go(lhs), go(rhs), sweet),
      CompoundAssignment: ({ lhs, op, rhs }): Expr => {
        const opMap: Record<CompoundAssignmentOperator, BinaryOperator> = {
          '+=': '+',
          '-=': '-',
          '*=': '*',
          '/=': '/',
          '%=': '%',
          '&&=': '&&',
          '||=': '||',
        };

        // syntactic sugar for: lhs = lhs op rhs
        const bitterLhs = go(lhs);
        return Expr.Assignment(
          bitterLhs,
          Expr.BinaryOp(
            bitterLhs,
            opMap[op],
            go(rhs),
            sweet
          ),
          sweet
        );
      },
      MethodCall: ({ receiver, method, args }) => Expr.MethodCall(go(receiver), method, args.map(arg => go(arg)), sweet),
      ModuleAccess: ({ path, member }) => Expr.ModuleAccess(path, member, sweet),
      FieldAccess: ({ lhs, field }) => Expr.FieldAccess(go(lhs), field, sweet),
      Tuple: ({ elements }) => Expr.Tuple(elements.map(e => go(e)), sweet),
      Match: ({ expr, cases }) => Expr.Match(go(expr), cases.map(c => ({ pattern: Pattern.fromSweet(c.pattern, nameEnv), body: go(c.body) })), sweet),
      Parenthesized: ({ expr }) => go(expr),
      NamedRecord: ({ name, typeParams, fields }) => Expr.NamedRecord(name, typeParams, fields.map(f => ({ name: f.name, value: go(f.value) })), sweet),
    });
  },
  showSweet: (expr: Expr): string => SweetExpr.show(expr.sweet),
};

export type Stmt = DataType<{
  Let: { name: Name, expr: Expr, mutable: boolean, annotation: Maybe<MonoTy> },
  Expr: { expr: Expr },
  Error: { message: string },
}>;

export const Stmt = {
  Let: (name: Name, expr: Expr, mutable: boolean, annotation: Maybe<MonoTy>): Stmt => ({ variant: 'Let', name, expr, mutable, annotation }),
  Expr: (expr: Expr): Stmt => ({ variant: 'Expr', expr }),
  Error: (message: string): Stmt => ({ variant: 'Error', message }),
  fromSweet: (sweet: SweetStmt, nameEnv: NameEnv, errors: BitterConversionError[]): Stmt => {
    return matchVariant(sweet, {
      Let: ({ name, expr, mutable, annotation }) => Stmt.Let(
        NameEnv.declare(nameEnv, name, mutable),
        Expr.fromSweet(expr, nameEnv, errors),
        mutable,
        annotation
      ),
      Expr: ({ expr }) => Stmt.Expr(Expr.fromSweet(expr, nameEnv, errors)),
      Error: ({ message }) => Stmt.Error(message),
    });
  },
};

type Field = { name: string, ty: ParameterizedTy };

export type Decl = DataType<{
  Function: {
    name: Name,
    args: { name: Name, mutable: boolean, annotation: Maybe<MonoTy> }[],
    body: Expr,
    funTy: PolyTy,
  },
  Module: {
    name: string,
    decls: Decl[],
    members: Record<string, Decl>,
  },
  TypeAlias: {
    name: string,
    typeParams: TypeParams,
    alias: ParameterizedTy,
  },
  Impl: {
    ty: ParameterizedTy,
    typeParams: TypeParams,
    decls: Decl[],
  },
  Error: { message: string },
}>;

export const Decl = {
  Function: (name: Name, args: { name: Name, mutable: boolean, annotation: Maybe<MonoTy> }[], body: Expr): Decl => ({ variant: 'Function', name, args, body, funTy: PolyTy.fresh() }),
  Module: (name: string, decls: Decl[]): Decl => {
    const mod: VariantOf<Decl, 'Module'> = {
      variant: 'Module',
      name,
      decls,
      members: {},
    };

    for (const decl of decls) {
      matchVariant(decl, {
        Function: func => {
          mod.members[func.name.original] = func;
        },
        Module: subMod => {
          mod.members[subMod.name] = subMod;
        },
        TypeAlias: alias => {
          mod.members[alias.name] = alias;
        },
        Impl: () => {

        },
        Error: () => { },
      });
    }

    return mod;
  },
  TypeAlias: (name: string, typeParams: TypeParams, alias: ParameterizedTy): Decl => ({ variant: 'TypeAlias', name, typeParams, alias }),
  Impl: (ty: ParameterizedTy, typeParams: TypeParams, decls: Decl[]): Decl => ({ variant: 'Impl', ty, typeParams, decls }),
  Error: (message: string): Decl => ({ variant: 'Error', message }),
  fromSweet: (sweet: SweetDecl, nameEnv: NameEnv, declareFuncNames: boolean, errors: BitterConversionError[]): Decl =>
    matchVariant(sweet, {
      Function: ({ name, args, body }) => {
        const withoutPatterns = removeFuncArgsPatternMatching(args, body, nameEnv, errors);

        return Decl.Function(
          declareFuncNames ? NameEnv.declare(nameEnv, name, false) : NameEnv.resolve(nameEnv, name),
          withoutPatterns.args,
          withoutPatterns.body
        );
      },
      Module: ({ name, decls }) => {
        const modEnv = NameEnv.clone(nameEnv);

        for (const decl of decls) {
          if (decl.variant === 'Function') {
            NameEnv.declare(modEnv, decl.name, false);
          }
        }

        return Decl.Module(
          name,
          decls.map(decl => Decl.fromSweet(decl, modEnv, false, errors))
        );
      },
      TypeAlias: ({ name, typeParams, alias }) => Decl.TypeAlias(name, typeParams, alias),
      Impl: ({ ty, typeParams, decls }) => {
        const implEnv = NameEnv.clone(nameEnv);

        for (const decl of decls) {
          if (decl.variant === 'Function') {
            NameEnv.declare(implEnv, decl.name, false);
          }
        }

        return Decl.Impl(ty, typeParams, decls.map(decl => Decl.fromSweet(decl, implEnv, false, errors)));
      },
      Error: ({ message }) => Decl.Error(message),
    }),
};

export type Prog = Decl[];

type BitterConversionError = string;

export const Prog = {
  fromSweet: (prog: SweetProg): [prog: Prog, errors: BitterConversionError[]] => {
    const nameEnv = NameEnv.make();

    // declare all function names beforehand
    // so that they can be used anywhere in the program
    for (const decl of prog) {
      if (decl.variant === 'Function') {
        NameEnv.declare(nameEnv, decl.name, false);
      }
    }

    const errors: BitterConversionError[] = [];

    const bitterProg = prog.map(decl => Decl.fromSweet(
      decl,
      nameEnv,
      false, // do not redeclare function names
      errors
    ));

    return [bitterProg, errors];
  },
};

export const removeFuncArgsPatternMatching = (
  args: SweetArgument[],
  body: SweetExpr,
  nameEnv: NameEnv,
  errors: string[],
): { args: Argument[], body: Expr } => {
  const firstRefuttableArg = args.find(({ pattern }) => !SweetPattern.isIrrefutable(pattern));
  if (firstRefuttableArg !== undefined) {
    errors.push(`patterns in function arguments must be irrefutable, but ${SweetPattern.show(firstRefuttableArg.pattern)} is not`);
  }

  // we don't have to do anything if all the patterns are variables or _
  if (args.every(arg => arg.pattern.variant === 'Variable' || arg.pattern.variant === 'Any')) {
    return {
      args: args.map(({ pattern, mutable, annotation }, index) => ({
        name: NameEnv.declare(nameEnv, pattern.variant === 'Variable' ? pattern.name : `_${index}`, mutable),
        mutable,
        annotation
      })),
      body: Expr.fromSweet(body, nameEnv, errors),
    };
  }

  // transform (p1, p2, ...) -> body into (arg1, arg2, ...) -> match { (p1, p2, ...) => body }
  // where arg1, arg2, ... are variables
  const bodyEnv = NameEnv.make();
  const varNames = args.map(({ mutable }, index) => NameEnv.declare(bodyEnv, `arg${index}`, mutable));
  const matchedExpr = args.length === 0 ?
    SweetExpr.Variable(varNames[0].original) :
    SweetExpr.Tuple(varNames.map(name => SweetExpr.Variable(name.original)));

  const pattern = args.length === 0 ?
    args[0].pattern :
    SweetPattern.Tuple(args.map(({ pattern }) => pattern));

  const newBody = Expr.fromSweet(
    SweetExpr.Match(matchedExpr, [{ pattern, body }]),
    bodyEnv,
    errors
  );

  return {
    args: varNames.map(arg => ({ name: arg, mutable: arg.mutable, annotation: none })),
    body: newBody,
  };
};