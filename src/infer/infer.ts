import { match as matchVariant, VariantOf } from 'itsamatch';
import { Decl, Expr, Prog, Stmt } from '../ast/bitter';
import { BinaryOperator, UnaryOperator } from '../ast/sweet';
import { Env } from './env';
import { MonoTy, PolyTy } from './types';
import { unify as unif } from './unification';

export type TypingError = string;

const unaryOpSignature: Record<UnaryOperator, PolyTy> = {
  '-': MonoTy.toPoly(MonoTy.TyFun([MonoTy.u32()], MonoTy.u32())),
  '!': MonoTy.toPoly(MonoTy.TyFun([MonoTy.bool()], MonoTy.bool())),
};

const binaryOpSignature: Record<BinaryOperator, PolyTy> = {
  '+': MonoTy.toPoly(MonoTy.TyFun([MonoTy.u32(), MonoTy.u32()], MonoTy.u32())),
  '-': MonoTy.toPoly(MonoTy.TyFun([MonoTy.u32(), MonoTy.u32()], MonoTy.u32())),
  '*': MonoTy.toPoly(MonoTy.TyFun([MonoTy.u32(), MonoTy.u32()], MonoTy.u32())),
  '/': MonoTy.toPoly(MonoTy.TyFun([MonoTy.u32(), MonoTy.u32()], MonoTy.u32())),
  '%': MonoTy.toPoly(MonoTy.TyFun([MonoTy.u32(), MonoTy.u32()], MonoTy.u32())),
  '==': MonoTy.toPoly(MonoTy.TyFun([MonoTy.u32(), MonoTy.u32()], MonoTy.bool())),
  '!=': MonoTy.toPoly(MonoTy.TyFun([MonoTy.u32(), MonoTy.u32()], MonoTy.bool())),
  '<': MonoTy.toPoly(MonoTy.TyFun([MonoTy.u32(), MonoTy.u32()], MonoTy.bool())),
  '>': MonoTy.toPoly(MonoTy.TyFun([MonoTy.u32(), MonoTy.u32()], MonoTy.bool())),
  '<=': MonoTy.toPoly(MonoTy.TyFun([MonoTy.u32(), MonoTy.u32()], MonoTy.bool())),
  '>=': MonoTy.toPoly(MonoTy.TyFun([MonoTy.u32(), MonoTy.u32()], MonoTy.bool())),
};

export const inferExpr = (expr: Expr, env: Env, errors: TypingError[]): TypingError[] => {
  const unify = (s: MonoTy, t: MonoTy): void => {
    errors.push(...unif(s, t).map(err => err + ' in ' + Expr.showSweet(expr)));
  };

  const tau = Expr.ty(expr);

  matchVariant(expr, {
    Const: () => { },
    Variable: ({ name }) => {
      Env.lookup(env, name.original).match({
        Some: ty => {
          const instTy = PolyTy.instantiate(ty);
          unify(instTy, tau);
        },
        None: () => {
          errors.push(`variable '${name.original}' is not in scope`);
        },
      });
    },
    UnaryOp: ({ op, expr }) => {
      inferExpr(expr, env, errors);
      const opTy1 = PolyTy.instantiate(unaryOpSignature[op]);
      const opTy2 = MonoTy.TyFun([Expr.ty(expr)], tau);
      unify(opTy1, opTy2);
    },
    BinaryOp: ({ lhs, op, rhs }) => {
      inferExpr(lhs, env, errors);
      inferExpr(rhs, env, errors);
      const opTy1 = PolyTy.instantiate(binaryOpSignature[op]);
      const opTy2 = MonoTy.TyFun([Expr.ty(lhs), Expr.ty(rhs)], tau);
      unify(opTy1, opTy2);
    },
    Call: ({ lhs, args }) => {
      inferExpr(lhs, env, errors);
      args.forEach(arg => {
        inferExpr(arg, env, errors);
      });
      const expectedFunTy = Expr.ty(lhs);
      const actualFunTy = MonoTy.TyFun(args.map(Expr.ty), tau);

      unify(expectedFunTy, actualFunTy);
    },
    Block: ({ statements }) => {
      const newEnv = { ...env };
      for (const stmt of statements) {
        inferStmt(stmt, newEnv, errors);
      }

      const last = statements[statements.length - 1];
      const lastTy = last ?
        (last.variant === 'Expr' ?
          Expr.ty(last.expr) : MonoTy.unit()
        ) : MonoTy.unit();

      unify(tau, lastTy);
    },
    IfThenElse: ({ condition, then, else_ }) => {
      inferExpr(condition, env, errors);
      inferExpr(then, env, errors);
      else_.match({
        Some: e => inferExpr(e, env, errors),
        None: () => { },
      });

      const condTy = Expr.ty(condition);
      unify(condTy, MonoTy.bool());
      const thenTy = Expr.ty(then);
      const elseTy = else_.mapWithDefault(Expr.ty, MonoTy.unit());
      unify(thenTy, tau);
      unify(elseTy, tau);
    },
    Assignment: ({ lhs, rhs }) => {
      inferExpr(lhs, env, errors);
      inferExpr(rhs, env, errors);
      const lhsTy = Expr.ty(lhs);
      const rhsTy = Expr.ty(rhs);
      unify(lhsTy, rhsTy);
      unify(tau, MonoTy.unit());
    },
    Closure: ({ args, body }) => {
      args.forEach(arg => {
        Env.addMono(env, arg.name.original, arg.name.ty);
      });

      inferExpr(body, env, errors);
      const retTy = Expr.ty(body);
      const funTy = MonoTy.TyFun(args.map(arg => arg.name.ty), retTy);
      unify(tau, funTy);
    },
    Error: ({ message }) => {
      errors.push(message);
    },
  });

  return errors;
};

export const inferStmt = (stmt: Stmt, env: Env, errors: TypingError[]): TypingError[] => {
  matchVariant(stmt, {
    Let: ({ name, expr }) => {
      Env.addMono(env, name.original, name.ty);
      inferExpr(expr, env, errors);
      const varTy = name.ty;
      const genTy = MonoTy.generalize(env, varTy);
      Env.addPoly(env, name.original, genTy);
    },
    Expr: ({ expr }) => {
      inferExpr(expr, env, errors);
    },
    Error: ({ message }) => {
      errors.push(message);
    },
  });

  return errors;
};

export const inferDecl = (decl: Decl, env: Env, errors: TypingError[]): TypingError[] => {
  matchVariant(decl, {
    Function: ({ name, args, body }) => {
      const bodyEnv = Env.clone(env);

      args.forEach(arg => {
        Env.addMono(bodyEnv, arg.name.original, arg.name.ty);
      });

      inferExpr(body, bodyEnv, errors);

      const funTy = MonoTy.TyFun(
        args.map(({ name: arg }) => arg.ty),
        Expr.ty(body)
      );

      errors.push(...unif(funTy, name.ty));

      const genFunTy = MonoTy.generalize(env, funTy);
      (decl as VariantOf<Decl, 'Function'>).funTy = genFunTy;
      Env.addPoly(env, name.original, genFunTy);
    },
    Error: ({ message }) => {
      errors.push(message);
    },
  });

  return errors;
};

export const infer = (prog: Prog): TypingError[] => {
  const errors: TypingError[] = [];
  const env: Env = Env.make();

  // declare all the functions so
  // that they can be called before being defined

  for (const decl of prog) {
    if (decl.variant === 'Function') {
      Env.addMono(env, decl.name.original, decl.name.ty);
    }
  }

  for (const decl of prog) {
    inferDecl(decl, env, errors);
  }

  console.log(Env.show(env));

  return errors;
};