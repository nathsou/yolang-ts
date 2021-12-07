import { DataType, match as matchVariant, VariantOf } from "itsamatch";
import { MonoTy, PolyTy } from "../infer/types";
import { Const } from "../parse/token";
import { Maybe } from "../utils/maybe";
import { BinaryOperator, CompoundAssignmentOperator, Decl as SweetDecl, Expr as SweetExpr, Prog as SweetProg, Stmt as SweetStmt, UnaryOperator } from "./sweet";

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

export type Expr = DataType<WithSweetRefAndType<{
  Const: { value: Const },
  Variable: { name: Name },
  Call: { lhs: Expr, args: Expr[] },
  BinaryOp: { lhs: Expr, op: BinaryOperator, rhs: Expr },
  UnaryOp: { op: UnaryOperator, expr: Expr },
  Error: { message: string },
  Closure: { args: { name: string, mutable: boolean }[], body: Expr },
  Block: { statements: Stmt[] },
  IfThenElse: { condition: Expr, then: Expr, else_: Maybe<Expr> },
  Assignment: { lhs: Expr, rhs: Expr },
  ModuleAccess: { path: string[], member: string },
}>>;

const typed = <T extends {}>(obj: T, sweet: SweetExpr): T & { ty: MonoTy, sweet: SweetExpr } => ({
  ...obj,
  sweet,
  ty: MonoTy.fresh(),
});

export const Expr = {
  Const: (c: Const, sweet: SweetExpr): Expr => ({
    variant: 'Const',
    value: c,
    sweet,
    ty: matchVariant(c, {
      u32: MonoTy.u32,
      bool: MonoTy.bool,
      unit: MonoTy.unit,
    }),
  }),
  Variable: (name: Name, sweet: SweetExpr): Expr => typed({ variant: 'Variable', name }, sweet),
  Call: (lhs: Expr, args: Expr[], sweet: SweetExpr): Expr => typed({ variant: 'Call', lhs, args }, sweet),
  BinaryOp: (lhs: Expr, op: BinaryOperator, rhs: Expr, sweet: SweetExpr): Expr => typed({ variant: 'BinaryOp', lhs, op, rhs }, sweet),
  UnaryOp: (op: UnaryOperator, expr: Expr, sweet: SweetExpr): Expr => typed({ variant: 'UnaryOp', op, expr }, sweet),
  Error: (message: string, sweet: SweetExpr): Expr => ({ variant: 'Error', message, sweet, ty: MonoTy.unit() }),
  Closure: (args: { name: string, mutable: boolean }[], body: Expr, sweet: SweetExpr): Expr => typed({ variant: 'Closure', args, body }, sweet),
  Block: (statements: Stmt[], sweet: SweetExpr): Expr => typed({ variant: 'Block', statements }, sweet),
  IfThenElse: (condition: Expr, then: Expr, else_: Maybe<Expr>, sweet: SweetExpr): Expr => typed({ variant: 'IfThenElse', condition, then, else_ }, sweet),
  Assignment: (lhs: Expr, rhs: Expr, sweet: SweetExpr): Expr => typed({ variant: 'Assignment', lhs, rhs }, sweet),
  ModuleAccess: (path: string[], member: string, sweet: SweetExpr): Expr => typed({ variant: 'ModuleAccess', path, member }, sweet),
  fromSweet: (sweet: SweetExpr, nameEnv: NameEnv): Expr =>
    matchVariant(sweet, {
      Const: ({ value }) => Expr.Const(value, sweet),
      Variable: ({ name }) => Expr.Variable(NameEnv.resolve(nameEnv, name), sweet),
      Call: ({ lhs, args }) => Expr.Call(Expr.fromSweet(lhs, nameEnv), args.map(arg => Expr.fromSweet(arg, nameEnv)), sweet),
      BinaryOp: ({ lhs, op, rhs }) => Expr.BinaryOp(Expr.fromSweet(lhs, nameEnv), op, Expr.fromSweet(rhs, nameEnv), sweet),
      UnaryOp: ({ op, expr }) => Expr.UnaryOp(op, Expr.fromSweet(expr, nameEnv), sweet),
      Error: ({ message }) => Expr.Error(message, sweet),
      Closure: ({ args, body }) => Expr.Closure(args, Expr.fromSweet(body, nameEnv), sweet),
      Block: ({ statements }) => {
        const newNameEnv = NameEnv.clone(nameEnv);
        return Expr.Block(statements.map(s => Stmt.fromSweet(s, newNameEnv)), sweet);
      },
      IfThenElse: ({ condition, then, else_ }) => Expr.IfThenElse(Expr.fromSweet(condition, nameEnv), Expr.fromSweet(then, nameEnv), else_.map(e => Expr.fromSweet(e, nameEnv)), sweet),
      Assignment: ({ lhs, rhs }) => Expr.Assignment(Expr.fromSweet(lhs, nameEnv), Expr.fromSweet(rhs, nameEnv), sweet),
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
        const bitterLhs = Expr.fromSweet(lhs, nameEnv);
        return Expr.Assignment(
          bitterLhs,
          Expr.BinaryOp(
            bitterLhs,
            opMap[op],
            Expr.fromSweet(rhs, nameEnv),
            sweet
          ),
          sweet
        );
      },
      ModuleAccess: ({ path, member }) => Expr.ModuleAccess(path, member, sweet),
    }),
  showSweet: (expr: Expr): string => SweetExpr.show(expr.sweet),
};

export type Stmt = DataType<{
  Let: { name: Name, expr: Expr, mutable: boolean },
  Expr: { expr: Expr },
  Error: { message: string },
}>;

export const Stmt = {
  Let: (name: Name, expr: Expr, mutable: boolean): Stmt => ({ variant: 'Let', name, expr, mutable }),
  Expr: (expr: Expr): Stmt => ({ variant: 'Expr', expr }),
  Error: (message: string): Stmt => ({ variant: 'Error', message }),
  fromSweet: (sweet: SweetStmt, nameEnv: NameEnv): Stmt => {
    return matchVariant(sweet, {
      Let: ({ name, expr, mutable }) => Stmt.Let(
        NameEnv.declare(nameEnv, name, mutable),
        Expr.fromSweet(expr, nameEnv),
        mutable
      ),
      Expr: ({ expr }) => Stmt.Expr(Expr.fromSweet(expr, nameEnv)),
      Error: ({ message }) => Stmt.Error(message),
    });
  },
};

export type Decl = DataType<{
  Function: {
    name: Name,
    args: { name: Name, mutable: boolean }[],
    body: Expr,
    funTy: PolyTy,
  },
  Module: {
    name: string,
    decls: Decl[],
    members: Record<string, Decl>,
  },
  Error: { message: string },
}>;

export const Decl = {
  Function: (name: Name, args: { name: Name, mutable: boolean }[], body: Expr): Decl => ({ variant: 'Function', name, args, body, funTy: PolyTy.fresh() }),
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
        Error: () => { },
      });
    }

    return mod;
  },
  Error: (message: string): Decl => ({ variant: 'Error', message }),
  fromSweet: (sweet: SweetDecl, nameEnv: NameEnv, declareFuncNames: boolean): Decl =>
    matchVariant(sweet, {
      Function: ({ name, args, body }) =>
        Decl.Function(
          declareFuncNames ? NameEnv.declare(nameEnv, name, false) : NameEnv.resolve(nameEnv, name),
          args.map(({ name: arg, mutable }) => ({ name: NameEnv.declare(nameEnv, arg, mutable), mutable })),
          Expr.fromSweet(body, nameEnv)
        ),
      Module: ({ name, decls }) => {
        const modEnv = NameEnv.clone(nameEnv);

        for (const decl of decls) {
          if (decl.variant === 'Function') {
            NameEnv.declare(modEnv, decl.name, false);
          }
        }

        return Decl.Module(
          name,
          decls.map(decl => Decl.fromSweet(decl, modEnv, false))
        );
      },
      Error: ({ message }) => Decl.Error(message),
    }),
};

export type Prog = Decl[];

export const Prog = {
  fromSweet: (prog: SweetProg): Prog => {
    const nameEnv = NameEnv.make();

    // declare all function names beforehand
    // so that they can be used anywhere in the program
    for (const decl of prog) {
      if (decl.variant === 'Function') {
        NameEnv.declare(nameEnv, decl.name, false);
      }
    }

    return prog.map(decl => Decl.fromSweet(
      decl,
      nameEnv,
      false // do not redeclare function names
    ));
  },
};