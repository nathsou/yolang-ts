import { DataType, match as matchVariant } from "itsamatch";
import { MonoTy } from "../infer/types";
import { Const } from "../parse/token";
import { Maybe } from "../utils/maybe";
import { Expr as SweetExpr, Stmt as SweetStmt, Decl as SweetDecl, BinaryOperator, UnaryOperator, CompoundAssignmentOperator } from "./sweet";

// Bitter expressions are *unsugared* representations
// of the structure of yolang source code
// with attached type information and lexically resolved identifier references

type Name = {
  readonly original: string,
  renaming: string,
  ty: MonoTy,
};

const Name = {
  fresh: (name: string): Name => ({
    original: name,
    renaming: name,
    ty: MonoTy.fresh(),
  }),
};

type NameEnv = Record<string, Name>;

const NameEnv = {
  make: (): NameEnv => ({}),
  clone: (env: NameEnv): NameEnv => ({ ...env }),
  declare: (env: NameEnv, name: string): Name => {
    const fresh = Name.fresh(name);
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

export type Expr = DataType<{
  Const: { value: Const, ty: MonoTy },
  Variable: { name: Name },
  Call: { name: Name, args: Expr[] },
  BinaryOp: { lhs: Expr, op: BinaryOperator, rhs: Expr, ty: MonoTy },
  UnaryOp: { op: UnaryOperator, expr: Expr, ty: MonoTy },
  Error: { message: string, ty: MonoTy },
  Closure: { args: { name: Name, mutable: boolean }[], body: Expr, ty: MonoTy },
  Block: { statements: Stmt[], ty: MonoTy },
  IfThenElse: { condition: Expr, then: Expr, else_: Maybe<Expr>, ty: MonoTy },
  Assignment: { lhs: Expr, rhs: Expr, ty: MonoTy },
}>;

const typed = <T extends {}>(obj: T): T & { ty: MonoTy } => ({
  ...obj,
  ty: MonoTy.fresh(),
});

export const Expr = {
  Const: (c: Const): Expr => typed({ variant: 'Const', value: c }),
  Variable: (name: Name): Expr => typed({ variant: 'Variable', name }),
  Call: (name: Name, args: Expr[]): Expr => typed({ variant: 'Call', name, args }),
  BinaryOp: (lhs: Expr, op: BinaryOperator, rhs: Expr): Expr => typed({ variant: 'BinaryOp', lhs, op, rhs }),
  UnaryOp: (op: UnaryOperator, expr: Expr): Expr => typed({ variant: 'UnaryOp', op, expr }),
  Error: (message: string): Expr => typed({ variant: 'Error', message }),
  Closure: (args: { name: Name, mutable: boolean }[], body: Expr): Expr => typed({ variant: 'Closure', args, body }),
  Block: (statements: Stmt[]): Expr => typed({ variant: 'Block', statements }),
  IfThenElse: (condition: Expr, then: Expr, else_: Maybe<Expr>): Expr => typed({ variant: 'IfThenElse', condition, then, else_ }),
  Assignment: (lhs: Expr, rhs: Expr): Expr => typed({ variant: 'Assignment', lhs, rhs }),
  fromSweet: (sweet: SweetExpr, nameEnv: NameEnv): Expr => {
    return matchVariant(sweet, {
      Const: ({ value }) => Expr.Const(value),
      Variable: ({ name }) => Expr.Variable(NameEnv.resolve(nameEnv, name)),
      Call: ({ name, args }) => Expr.Call(NameEnv.resolve(nameEnv, name), args.map(arg => Expr.fromSweet(arg, nameEnv))),
      BinaryOp: ({ lhs, op, rhs }) => Expr.BinaryOp(Expr.fromSweet(lhs, nameEnv), op, Expr.fromSweet(rhs, nameEnv)),
      UnaryOp: ({ op, expr }) => Expr.UnaryOp(op, Expr.fromSweet(expr, nameEnv)),
      Error: ({ message }) => Expr.Error(message),
      Closure: ({ args, body }) => Expr.Closure(args.map(({ name, mutable }) => ({ name: NameEnv.resolve(nameEnv, name), mutable })), Expr.fromSweet(body, nameEnv)),
      Block: ({ statements }) => {
        const newNameEnv = NameEnv.clone(nameEnv);
        return Expr.Block(statements.map(s => Stmt.fromSweet(s, newNameEnv)));
      },
      IfThenElse: ({ condition, then, else_ }) => Expr.IfThenElse(Expr.fromSweet(condition, nameEnv), Expr.fromSweet(then, nameEnv), else_.map(e => Expr.fromSweet(e, nameEnv))),
      Assignment: ({ lhs, rhs }) => Expr.Assignment(Expr.fromSweet(lhs, nameEnv), Expr.fromSweet(rhs, nameEnv)),
      CompoundAssignment: ({ lhs, op, rhs }): Expr => {
        const opMap: Record<CompoundAssignmentOperator, BinaryOperator> = {
          '+=': '+',
          '-=': '-',
          '*=': '*',
          '/=': '/',
          '%=': '%',
        };

        // syntactic sugar for: lhs = lhs op rhs
        const bitterLhs = Expr.fromSweet(lhs, nameEnv);
        return Expr.Assignment(
          bitterLhs,
          Expr.BinaryOp(
            bitterLhs,
            opMap[op],
            Expr.fromSweet(rhs, nameEnv)
          )
        );
      },
    });
  },
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
        NameEnv.declare(nameEnv, name),
        Expr.fromSweet(expr, nameEnv),
        mutable
      ),
      Expr: ({ expr }) => Stmt.Expr(Expr.fromSweet(expr, nameEnv)),
      Error: ({ message }) => Stmt.Error(message),
    });
  },
};

export type Decl = DataType<{
  Function: { name: Name, args: { name: Name, mutable: boolean }[], body: Expr },
  Error: { message: string },
}>;

export const Decl = {
  Function: (name: Name, args: { name: Name, mutable: boolean }[], body: Expr): Decl => ({ variant: 'Function', name, args, body }),
  Error: (message: string): Decl => ({ variant: 'Error', message }),
  fromSweet: (sweet: SweetDecl, nameEnv: NameEnv): Decl =>
    matchVariant(sweet, {
      Function: ({ name, args, body }) =>
        Decl.Function(
          NameEnv.declare(nameEnv, name),
          args.map(({ name: arg, mutable }) => ({ name: NameEnv.declare(nameEnv, arg), mutable })),
          Expr.fromSweet(body, nameEnv)
        ),
      Error: ({ message }) => Decl.Error(message),
    }),
};