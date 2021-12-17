import { match as matchVariant, VariantOf } from 'itsamatch';
import { Decl, Expr, Pattern, Prog, Stmt } from '../ast/bitter';
import { BinaryOperator, UnaryOperator } from '../ast/sweet';
import { Maybe, none, some } from '../utils/maybe';
import { proj } from '../utils/misc';
import { Env } from './env';
import { Row } from './records';
import { MonoTy, PolyTy } from './types';
import { unify as unif } from './unification';

export type TypingError = string;

const unaryOpSignature: Record<UnaryOperator, PolyTy> = {
  '-': MonoTy.toPoly(MonoTy.TyFun([MonoTy.u32()], MonoTy.u32())),
  '!': MonoTy.toPoly(MonoTy.TyFun([MonoTy.bool()], MonoTy.bool())),
};

const u32OpSig = MonoTy.toPoly(MonoTy.TyFun([MonoTy.u32(), MonoTy.u32()], MonoTy.u32()));
const u32BoolOpSig = MonoTy.toPoly(MonoTy.TyFun([MonoTy.u32(), MonoTy.u32()], MonoTy.bool()));
const comparisonOpSig = PolyTy.make(
  [0],
  MonoTy.TyFun([MonoTy.TyVar({ kind: 'Var', id: 0 }), MonoTy.TyVar({ kind: 'Var', id: 0 })], MonoTy.bool())
);

const logicalOpSig = MonoTy.toPoly(MonoTy.TyFun([MonoTy.bool(), MonoTy.bool()], MonoTy.bool()));

const binaryOpSignature: Record<BinaryOperator, PolyTy> = {
  '+': u32OpSig,
  '-': u32OpSig,
  '*': u32OpSig,
  '/': u32OpSig,
  '%': u32OpSig,
  '==': comparisonOpSig,
  '!=': comparisonOpSig,
  '<': u32BoolOpSig,
  '>': u32BoolOpSig,
  '<=': u32BoolOpSig,
  '>=': u32BoolOpSig,
  '&&': logicalOpSig,
  '||': logicalOpSig,
};

type TypeContext = {
  env: Env,
  modules: Record<string, VariantOf<Decl, 'Module'>>,
  typeDecls: Record<string, VariantOf<Decl, 'Struct'>>,
};

const TypeContext = {
  make: (): TypeContext => ({ env: Env.make(), modules: {}, typeDecls: {} }),
  clone: (ctx: TypeContext) => ({ env: Env.clone(ctx.env), modules: { ...ctx.modules }, typeDecls: { ...ctx.typeDecls } }),
  declareModule: (ctx: TypeContext, mod: VariantOf<Decl, 'Module'>): void => {
    ctx.modules[mod.name] = mod;
  },
  declareType: (ctx: TypeContext, ty: VariantOf<Decl, 'Struct'>): void => {
    ctx.typeDecls[ty.name] = ty;
  },
  // TODO: cleanup
  resolveModule: (ctx: TypeContext, path: string[]): Maybe<VariantOf<Decl, 'Module'>> => {
    let members: Record<string, Decl> = ctx.modules;
    let mod: VariantOf<Decl, 'Module'> | undefined;

    for (const name of path) {
      const decl = members[name];
      if (decl !== undefined && decl.variant === 'Module') {
        mod = decl;
        members = mod.members;
      } else {
        return none;
      }
    }

    return mod ? some(mod) : none;
  },
};

export const inferExpr = (
  expr: Expr,
  ctx: TypeContext,
  errors: TypingError[]
): TypingError[] => {
  const unify = (s: MonoTy, t: MonoTy): void => {
    errors.push(...unif(s, t).map(err => err + ' in ' + Expr.showSweet(expr)));
  };

  const { env } = ctx;
  const tau = expr.ty;

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
      inferExpr(expr, ctx, errors);
      const opTy1 = PolyTy.instantiate(unaryOpSignature[op]);
      const opTy2 = MonoTy.TyFun([expr.ty], tau);
      unify(opTy1, opTy2);
    },
    BinaryOp: ({ lhs, op, rhs }) => {
      inferExpr(lhs, ctx, errors);
      inferExpr(rhs, ctx, errors);
      const opTy1 = PolyTy.instantiate(binaryOpSignature[op]);
      const opTy2 = MonoTy.TyFun([lhs.ty, rhs.ty], tau);
      unify(opTy1, opTy2);
    },
    Call: ({ lhs, args }) => {
      inferExpr(lhs, ctx, errors);
      args.forEach(arg => {
        inferExpr(arg, ctx, errors);
      });
      const expectedFunTy = lhs.ty;
      const actualFunTy = MonoTy.TyFun(args.map(proj('ty')), tau);

      unify(expectedFunTy, actualFunTy);
    },
    Block: ({ statements }) => {
      const newCtx = TypeContext.clone(ctx);
      for (const stmt of statements) {
        inferStmt(stmt, newCtx, errors);
      }

      const last = statements[statements.length - 1];
      const lastTy = last ?
        (last.variant === 'Expr' ?
          last.expr.ty : MonoTy.unit()
        ) : MonoTy.unit();

      unify(tau, lastTy);
    },
    IfThenElse: ({ condition, then, else_ }) => {
      inferExpr(condition, ctx, errors);
      inferExpr(then, ctx, errors);
      else_.match({
        Some: e => inferExpr(e, ctx, errors),
        None: () => { },
      });

      const condTy = condition.ty;
      unify(condTy, MonoTy.bool());
      const thenTy = then.ty;
      const elseTy = else_.mapWithDefault(proj('ty'), MonoTy.unit());
      unify(thenTy, tau);
      unify(elseTy, tau);
    },
    Assignment: ({ lhs, rhs }) => {
      inferExpr(lhs, ctx, errors);
      inferExpr(rhs, ctx, errors);
      const lhsTy = lhs.ty;
      const rhsTy = rhs.ty;
      unify(lhsTy, rhsTy);
      unify(tau, MonoTy.unit());

      // mutability check
      if (lhs.variant === 'Variable') {
        if (!lhs.name.mutable) {
          errors.push(`variable '${lhs.name.original}' is not mutable`);
        }
      } else {
        errors.push(`cannot assign to ${Expr.showSweet(lhs)}`);
      }
    },
    Closure: ({ args, body }) => {
      const bodyCtx = TypeContext.clone(ctx);

      args.forEach(arg => {
        Env.addMono(bodyCtx.env, arg.name.original, arg.name.ty);
      });

      inferExpr(body, bodyCtx, errors);

      const funTy = MonoTy.TyFun(
        args.map(({ name: arg }) => arg.ty),
        body.ty
      );

      unify(tau, funTy);
    },
    ModuleAccess: ({ path, member }) => {
      TypeContext.resolveModule(ctx, path).match({
        Some: mod => {
          if (member in mod.members) {
            const m = mod.members[member];
            matchVariant(m, {
              Function: ({ funTy }) => {
                const instTy = PolyTy.instantiate(funTy);
                unify(instTy, tau);
              },
              _: () => {
                errors.push(`member '${path.join('.')}.${member}' is not a function`);
              },
            });
          } else {
            errors.push(`'${member}' is not a member of module '${path.join('.')}'`);
          }
        },
        None: () => {
          errors.push(`module '${path.join('.')}' is not in scope`);
        },
      });
    },
    Tuple: ({ elements }) => {
      elements.forEach(e => {
        inferExpr(e, ctx, errors);
      });

      const elemTys = elements.map(proj('ty'));
      unify(tau, MonoTy.tuple(elemTys));
    },
    Match: ({ expr, cases }) => {
      // match e { p1 => e1, p2 => e2, ... }
      const retTy = MonoTy.fresh();
      const argTy = expr.ty;

      inferExpr(expr, ctx, errors);

      if (cases.length === 0) {
        unify(retTy, MonoTy.unit());
      } else {
        for (const { pattern, body } of cases) {
          unify(argTy, Pattern.type(pattern));

          const bodyCtx = TypeContext.clone(ctx);

          for (const v of Pattern.vars(pattern)) {
            Env.addMono(bodyCtx.env, v.original, v.ty);
          }

          inferExpr(body, bodyCtx, errors);

          unify(retTy, body.ty);
        }
      }

      unify(tau, retTy);
    },
    FieldAccess: ({ lhs, field }) => {
      const rowTail = MonoTy.fresh();
      const partialRecordTy = MonoTy.TyRecord(Row.extend(field, tau, rowTail));

      inferExpr(lhs, ctx, errors);
      unify(partialRecordTy, lhs.ty);
    },
    Error: ({ message }) => {
      errors.push(message);
    },
  });

  return errors;
};

export const inferPattern = (pat: Pattern, expr: Expr, ctx: TypeContext, errors: TypingError[]): TypingError[] => {
  const unify = (s: MonoTy, t: MonoTy): void => {
    errors.push(...unif(s, t).map(err => err + ' in ' + Expr.showSweet(expr)));
  };

  const tau = expr.ty;

  unify(Pattern.type(pat), tau);

  return errors;
};

export const inferStmt = (stmt: Stmt, ctx: TypeContext, errors: TypingError[]): TypingError[] => {
  matchVariant(stmt, {
    Let: ({ name, expr }) => {
      inferExpr(expr, ctx, errors);
      const genTy = MonoTy.generalize(ctx.env, expr.ty);
      Env.addPoly(ctx.env, name.original, genTy);
    },
    Expr: ({ expr }) => {
      inferExpr(expr, ctx, errors);
    },
    Error: ({ message }) => {
      errors.push(message);
    },
  });

  return errors;
};

export const inferDecl = (decl: Decl, ctx: TypeContext, errors: TypingError[]): TypingError[] => {
  matchVariant(decl, {
    Function: func => {
      const { name, args, body } = func;
      const bodyCtx = TypeContext.clone(ctx);

      args.forEach(arg => {
        Env.addMono(bodyCtx.env, arg.name.original, arg.name.ty);
      });

      inferExpr(body, bodyCtx, errors);

      const funTy = MonoTy.TyFun(
        args.map(({ name: arg }) => arg.ty),
        body.ty
      );

      errors.push(...unif(funTy, name.ty));

      const genFunTy = MonoTy.generalize(ctx.env, funTy);
      func.funTy = genFunTy;
      Env.addPoly(ctx.env, name.original, genFunTy);
    },
    Module: mod => {
      TypeContext.declareModule(ctx, mod);
      const modCtx = TypeContext.clone(ctx);

      // declare all the functions and modules so
      // that they can be used before being defined
      for (const decl of mod.decls) {
        matchVariant(decl, {
          Function: ({ name, funTy }) => {
            Env.addPoly(modCtx.env, name.original, funTy);
          },
          Module: subMod => {
            inferDecl(subMod, modCtx, errors);
          },
          _: () => { },
        });
      }

      for (const decl of mod.decls) {
        inferDecl(decl, modCtx, errors);
      }
    },
    Struct: struct => {
      TypeContext.declareType(ctx, struct);
    },
    Error: ({ message }) => {
      errors.push(message);
    },
  });

  return errors;
};

export const infer = (prog: Prog): TypingError[] => {
  const errors: TypingError[] = [];
  const topModule = Decl.Module('top', prog);
  const ctx = TypeContext.make();

  inferDecl(topModule, ctx, errors);

  return errors;
};