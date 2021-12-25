import { match as matchVariant } from 'itsamatch';
import { Decl, Expr, Pattern, Prog, Stmt } from '../ast/bitter';
import { some } from '../utils/maybe';
import { proj } from '../utils/misc';
import { Env } from './env';
import { RowMono } from './records';
import { signatures } from './signatures';
import { TypeContext } from './typeContext';
import { MonoTy, PolyTy } from './types';
import { unifyMut } from './unification';

export type TypingError = string;

export const inferExpr = (
  expr: Expr,
  ctx: TypeContext,
  errors: TypingError[]
): TypingError[] => {
  const unify = (s: MonoTy, t: MonoTy): void => {
    errors.push(...unifyMut(s, t).map(err => err + ' in ' + Expr.showSweet(expr)));
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
      const opTy1 = PolyTy.instantiate(signatures.unaryOp[op]);
      const opTy2 = MonoTy.Fun([expr.ty], tau);
      unify(opTy1, opTy2);
    },
    BinaryOp: ({ lhs, op, rhs }) => {
      inferExpr(lhs, ctx, errors);
      inferExpr(rhs, ctx, errors);
      const opTy1 = PolyTy.instantiate(signatures.binaryOp[op]);
      const opTy2 = MonoTy.Fun([lhs.ty, rhs.ty], tau);
      unify(opTy1, opTy2);
    },
    Call: ({ lhs, args }) => {
      inferExpr(lhs, ctx, errors);
      args.forEach(arg => {
        inferExpr(arg, ctx, errors);
      });
      const expectedFunTy = lhs.ty;
      const actualFunTy = MonoTy.Fun(args.map(proj('ty')), tau);

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
      else_.do(e => {
        inferExpr(e, ctx, errors);
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
        arg.annotation.do(ann => {
          unify(arg.name.ty, ann);
        });
      });

      inferExpr(body, bodyCtx, errors);

      const funTy = MonoTy.Fun(
        args.map(({ name: arg }) => arg.ty),
        body.ty
      );

      unify(tau, funTy);
    },
    MethodCall: expr => {
      const { receiver, method, args } = expr;
      inferExpr(receiver, ctx, errors);
      args.forEach(arg => {
        inferExpr(arg, ctx, errors);
      });

      TypeContext.findImplMethod(ctx, method, receiver.ty).match({
        Some: ([impl, _subst, implInstTy]) => {
          const isMethod = method in impl.methods;
          if (!isMethod) {
            errors.push(`no method '${method}' found for type ${MonoTy.show(receiver.ty)}`);
          } else {
            expr.impl = some(impl);

            unify(implInstTy, receiver.ty);
            const func = impl.methods[method];

            args.forEach(arg => {
              inferExpr(arg, ctx, errors);
            });

            const expectedFunTy = MonoTy.Fun(func.args.map(arg => arg.name.ty), func.body.ty);

            const argsTysWithSelf = [receiver.ty, ...args.map(proj('ty'))];
            const actualFunTy = MonoTy.Fun(argsTysWithSelf, tau);

            unify(expectedFunTy, actualFunTy);
            unify(func.body.ty, tau);
          }
        },
        None: () => {
          errors.push(`no method '${method}' found for type ${MonoTy.show(receiver.ty)}`);
        },
      });
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
      const partialRecordTy = MonoTy.Record(RowMono.extend(field, tau, rowTail));

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
    errors.push(...unifyMut(s, t).map(err => err + ' in ' + Expr.showSweet(expr)));
  };

  const tau = expr.ty;

  unify(Pattern.type(pat), tau);

  return errors;
};

export const inferStmt = (stmt: Stmt, ctx: TypeContext, errors: TypingError[]): TypingError[] => {
  matchVariant(stmt, {
    Let: ({ name, expr, annotation }) => {
      inferExpr(expr, ctx, errors);

      annotation.do(ann => {
        errors.push(...unifyMut(expr.ty, ann));
      });

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

export const inferDecl = (decl: Decl, ctx: TypeContext, declare: boolean, errors: TypingError[]): TypingError[] => {
  matchVariant(decl, {
    Function: func => {
      const { name, args, body } = func;
      const bodyCtx = TypeContext.clone(ctx);

      args.forEach(arg => {
        Env.addMono(bodyCtx.env, arg.name.original, arg.name.ty);
        arg.annotation.do(ann => {
          errors.push(...unifyMut(arg.name.ty, ann));
        });
      });

      inferExpr(body, bodyCtx, errors);

      const funTy = MonoTy.Fun(
        args.map(({ name: arg }) => arg.ty),
        body.ty
      );

      errors.push(...unifyMut(funTy, name.ty));

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
            inferDecl(subMod, modCtx, declare, errors);
          },
          Impl: impl => {
            inferDecl(impl, modCtx, declare, errors);
          },
          _: () => { },
        });
      }

      for (const decl of mod.decls) {
        inferDecl(decl, modCtx, false, errors);
      }
    },
    NamedRecord: record => {
      if (declare) {
        TypeContext.declareType(ctx, record);
      }
    },
    Impl: impl => {
      if (declare) {
        TypeContext.declareImpl(ctx, impl);

        const implCtx = TypeContext.clone(ctx);

        for (const decl of impl.decls) {
          inferDecl(decl, implCtx, declare, errors);
        }
      }
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

  inferDecl(topModule, ctx, true, errors);

  return errors;
};