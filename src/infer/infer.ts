import { DataType, match as matchVariant, VariantOf } from 'itsamatch';
import { Decl, Expr, Pattern, Prog, Stmt } from '../ast/bitter';
import { Error } from '../errors/errors';
import { gen } from '../utils/array';
import { some } from '../utils/maybe';
import { proj } from '../utils/misc';
import { Env } from './env';
import { Row } from './records';
import { signatures } from './signatures';
import { MAX_TUPLE_INDEX, Tuple } from './tuples';
import { TypeContext } from './typeContext';
import { MonoTy, PolyTy } from './types';
import { unifyMut } from './unification';

export type TypingError = DataType<{
  UnboundVariable: { name: string },
  ImmutableVariable: { name: string },
  UnassignableExpression: { expr: Expr },
  UnknownMethod: { method: string, ty: MonoTy },
  UnknownModule: { path: string[] },
  UnknownModuleMember: { path: string[], member: string },
  TEMP_OnlyFunctionModuleAccessAllowed: { path: string[], member: string },
  TupleIndexTooBig: { index: number },
  ParsingError: { message: string },
}, 'type'>;

export const inferExpr = (
  expr: Expr,
  ctx: TypeContext,
  errors: Error[]
): Error[] => {
  const unify = (s: MonoTy, t: MonoTy): void => {
    errors.push(...unifyMut(s, t, ctx));
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
          errors.push(Error.Typing({ type: 'UnboundVariable', name: name.original }));
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
          errors.push(Error.Typing({ type: 'ImmutableVariable', name: lhs.name.original }));
        }
      } else {
        errors.push(Error.Typing({ type: 'UnassignableExpression', expr: lhs }));
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
            errors.push(Error.Typing({ type: 'UnknownMethod', method, ty: receiver.ty }));
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
          errors.push(Error.Typing({ type: 'UnknownMethod', method, ty: receiver.ty }));
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
                errors.push(Error.Typing({ type: 'TEMP_OnlyFunctionModuleAccessAllowed', path, member }));
              },
            });
          } else {
            errors.push(Error.Typing({ type: 'UnknownModuleMember', path, member }));
          }
        },
        None: () => {
          errors.push(Error.Typing({ type: 'UnknownModule', path }));
        },
      });
    },
    Tuple: ({ elements }) => {
      elements.forEach(e => {
        inferExpr(e, ctx, errors);
      });

      const elemTys = elements.map(proj('ty'));
      unify(tau, MonoTy.Tuple(Tuple.fromArray(elemTys)));
    },
    Match: ({ expr, annotation, cases }) => {
      // match e { p1 => e1, p2 => e2, ... }
      const retTy = MonoTy.fresh();
      const argTy = expr.ty;

      annotation.do(ann => {
        unify(argTy, ann);
      });

      inferExpr(expr, ctx, errors);

      if (cases.length === 0) {
        unify(retTy, MonoTy.unit());
      } else {
        for (const { pattern, annotation, body } of cases) {
          const patternTy = Pattern.type(pattern);
          unify(argTy, patternTy);

          annotation.do(ann => {
            unify(ann, patternTy);
          });

          const bodyCtx = TypeContext.clone(ctx);

          for (const v of Pattern.vars(pattern)) {
            const genTy = MonoTy.generalize(ctx.env, v.ty);
            Env.addPoly(bodyCtx.env, v.original, genTy);
          }

          inferExpr(body, bodyCtx, errors);

          unify(retTy, body.ty);
        }
      }

      unify(tau, retTy);
    },
    FieldAccess: ({ lhs, field }) => {
      const rowTail = MonoTy.fresh();
      const partialRecordTy = MonoTy.Record(Row.extend(field, tau, rowTail));

      inferExpr(lhs, ctx, errors);
      unify(partialRecordTy, lhs.ty);
    },
    NamedRecord: ({ path, name, typeParams, fields }) => {
      const expectedTy = MonoTy.ConstWithPath(path, name, ...typeParams);

      fields.forEach(({ value }) => {
        inferExpr(value, ctx, errors);
      });

      const actualTy = MonoTy.Record(
        Row.fromFields(fields.map(f => [f.name, f.value.ty]))
      );

      unify(expectedTy, actualTy);
      unify(tau, expectedTy);
    },
    TupleIndexing: ({ lhs, index }) => {
      if (index > MAX_TUPLE_INDEX) {
        errors.push(Error.Typing({ type: 'TupleIndexTooBig', index }));
      } else {
        inferExpr(lhs, ctx, errors);
        const elemsTys = gen(index + 1, MonoTy.fresh);
        const expectedLhsTy = MonoTy.Tuple(Tuple.fromArray(elemsTys, true));
        const actualLhsTy = lhs.ty;

        unify(expectedLhsTy, actualLhsTy);
        unify(tau, elemsTys[index]);
      }
    },
    Error: ({ message }) => {
      errors.push(Error.Typing({ type: 'ParsingError', message }));
    },
  });

  return errors;
};

export const inferPattern = (pat: Pattern, expr: Expr, ctx: TypeContext, errors: Error[]): Error[] => {
  const unify = (s: MonoTy, t: MonoTy): void => {
    errors.push(...unifyMut(s, t, ctx));
  };

  const tau = expr.ty;

  unify(Pattern.type(pat), tau);

  return errors;
};

export const inferStmt = (stmt: Stmt, ctx: TypeContext, errors: Error[]): Error[] => {
  matchVariant(stmt, {
    Let: ({ name, expr, annotation }) => {
      inferExpr(expr, ctx, errors);

      annotation.do(ann => {
        errors.push(...unifyMut(expr.ty, ann, ctx));
      });

      const genTy = MonoTy.generalize(ctx.env, expr.ty);
      Env.addPoly(ctx.env, name.original, genTy);
    },
    Expr: ({ expr }) => {
      inferExpr(expr, ctx, errors);
    },
    Error: ({ message }) => {
      errors.push(Error.Typing({ type: 'ParsingError', message }));
    },
  });

  return errors;
};

export const inferDecl = (decl: Decl, ctx: TypeContext, declare: boolean, errors: Error[]): Error[] => {
  matchVariant(decl, {
    Function: func => {
      const { name, typeParams, args, body } = func;
      const bodyCtx = TypeContext.clone(ctx);
      TypeContext.declareTypeParams(bodyCtx, ...typeParams);

      args.forEach(arg => {
        arg.annotation.do(ann => {
          errors.push(...unifyMut(arg.name.ty, ann, bodyCtx));
        });

        Env.addMono(bodyCtx.env, arg.name.original, arg.name.ty);
      });

      inferExpr(body, bodyCtx, errors);

      const funTy = MonoTy.Fun(
        args.map(({ name: arg }) => arg.ty),
        body.ty
      );

      errors.push(...unifyMut(funTy, name.ty, ctx));

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
          _: decl => {
            inferDecl(decl, modCtx, true, errors);
          },
        });
      }

      for (const decl of mod.decls) {
        inferDecl(decl, modCtx, false, errors);
      }
    },
    TypeAlias: ({ name, typeParams, alias }) => {
      if (declare) {
        TypeContext.declareTypeAlias(ctx, name, typeParams, alias);
      }
    },
    Impl: impl => {
      if (declare) {
        TypeContext.declareImpl(ctx, impl);

        const implCtx = TypeContext.clone(ctx);
        TypeContext.declareTypeParams(implCtx, ...impl.typeParams);

        for (const decl of impl.decls) {
          inferDecl(decl, implCtx, declare, errors);
        }
      }
    },
    Trait: () => {

    },
    Error: ({ message }) => {
      errors.push(Error.Typing({ type: 'ParsingError', message }));
    },
  });

  return errors;
};

export const infer = (prog: Prog): Error[] => {
  const errors: Error[] = [];
  const ctx = TypeContext.make(prog);

  for (const decl of prog) {
    inferDecl(decl, ctx, true, errors);
  }

  return errors;
};