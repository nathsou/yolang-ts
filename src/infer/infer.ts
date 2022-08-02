import { DataType, genConstructors, match as matchVariant } from 'itsamatch';
import { Decl, Expr, Pattern, Prog, Stmt } from '../ast/bitter';
import { Name } from '../ast/name';
import { Inst } from '../codegen/wasm/instructions';
import { ValueType } from '../codegen/wasm/types';
import { wasmTy } from '../codegen/wasm/utils';
import { Error } from '../errors/errors';
import { gen, zip } from '../utils/array';
import { Maybe, none, some } from '../utils/maybe';
import { panic, proj } from '../utils/misc';
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
  WasmStackUnderflow: { expectedLength: number, actualLength: number, inst: Inst },
  WasmStackOverflow: { expectedLength: number, actualLength: number },
  InconsistentWasmStack: { expectedTy: ValueType, actualTy: ValueType, inst: Inst },
  WasmBlockExpressionsRequireTypeAnnotations: { expr: Expr },
}, 'type'>;

const inferExpr = (
  expr: Expr,
  ctx: TypeContext,
  errors: Error[]
): Error[] => {
  const unify = (s: MonoTy, t: MonoTy): void => {
    errors.push(...unifyMut(s, t, ctx));
  };

  const { env } = ctx;
  const tau = expr.ty;
  expr.typeContext = ctx;

  const resolveVar = (name: Name): Maybe<PolyTy> => {
    return Env.lookup(env, name.original).match({
      Some: ({ ty, name: declaredName }) => {
        if (name.isUndeclared) {
          name = declaredName;
        }

        return some(ty);
      },
      None: () => {
        errors.push(Error.Typing({ type: 'UnboundVariable', name: name.original }));
        return none;
      },
    });
  };

  matchVariant(expr, {
    Const: () => { },
    Variable: v => {
      resolveVar(v.name).do(ty => {
        const instTy = PolyTy.instantiate(ty);
        unify(instTy.ty, tau);
      });
    },
    UnaryOp: ({ op, expr }) => {
      inferExpr(expr, ctx, errors);
      const opTy1 = PolyTy.instantiate(signatures.unaryOp[op]);
      const opTy2 = MonoTy.Fun([expr.ty], tau);
      unify(opTy1.ty, opTy2);
    },
    BinaryOp: ({ lhs, op, rhs }) => {
      inferExpr(lhs, ctx, errors);
      inferExpr(rhs, ctx, errors);
      const opTy1 = PolyTy.instantiate(signatures.binaryOp[op]);
      const opTy2 = MonoTy.Fun([lhs.ty, rhs.ty], tau);
      unify(opTy1.ty, opTy2);
    },
    NamedFuncCall: f => {
      const { name, typeParams, args } = f;
      resolveVar(name).do(ty => {
        args.forEach(arg => {
          inferExpr(arg, ctx, errors);
        });

        const partiallyInstTy = PolyTy.instantiatePartially(ty, typeParams);
        const expectedFunTy = PolyTy.instantiate(partiallyInstTy);

        const allTyVars = [...typeParams, ...partiallyInstTy[0].map(t => expectedFunTy.subst.get(t)!)];
        const actualFunTy = MonoTy.Fun(args.map(proj('ty')), tau);

        unify(expectedFunTy.ty, actualFunTy);
        f.typeParams = allTyVars;
      });
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
    Block: ({ statements, lastExpr }) => {
      const newCtx = TypeContext.clone(ctx);

      statements.forEach(stmt => {
        inferStmt(stmt, newCtx, errors);
      });

      lastExpr.do(expr => {
        inferExpr(expr, newCtx, errors);
      });

      const retTy = lastExpr.mapWithDefault(proj('ty'), MonoTy.unit());
      unify(tau, retTy);
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

      args.forEach(({ name, annotation }) => {
        Env.addMono(bodyCtx.env, name, name.ty);
        annotation.do(ann => {
          unify(name.ty, ann);
        });
      });

      inferExpr(body, bodyCtx, errors);

      const funTy = MonoTy.Fun(
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
                unify(instTy.ty, tau);
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
            Env.addPoly(bodyCtx.env, v, genTy);
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
    WasmBlock: ({ instructions }) => {
      const stack: ValueType[] = [];

      instructions.forEach(inst => {
        inst.match({
          left: inst => {
            const { consumes, outputs } = Inst.type(inst);
            if (consumes.length > stack.length) {
              errors.push(Error.Typing({
                type: 'WasmStackUnderflow',
                expectedLength: consumes.length,
                actualLength: stack.length,
                inst,
              }));
            } else {
              const consumed = stack.splice(stack.length - consumes.length, consumes.length);
              zip(consumes, consumed).forEach(([expectedTy, actualTy]) => {
                if (!ValueType.eq(expectedTy, actualTy)) {
                  errors.push(Error.Typing({
                    type: 'InconsistentWasmStack',
                    expectedTy,
                    actualTy,
                    inst,
                  }));
                }
              });

              stack.push(...outputs);
            }
          },
          right: ([expr, ann]) => {
            inferExpr(expr, ctx, errors);

            ann.do(ann => {
              unify(expr.ty, ann);
            });

            if (!MonoTy.isDetermined(expr.ty)) {
              errors.push(Error.Typing({
                type: 'WasmBlockExpressionsRequireTypeAnnotations',
                expr,
              }));

              return;
            }

            const ty = wasmTy(expr.ty);
            if (!ValueType.eq(ty, ValueType.none)) {
              stack.push(ty);
            }
          },
        });
      });

      if (stack.length > 1) {
        errors.push(Error.Typing({
          type: 'WasmStackOverflow',
          expectedLength: 1,
          actualLength: stack.length
        }));
      }

      if (stack.length > 0 && stack[0] !== ValueType.i32) {
        panic(`ValueType ${stack[0]} is not supported yet in wasm blocks`);
      }

      // TODO: update when we have other numeric types
      const ty = stack.length === 0 || stack[0] === ValueType.none ? MonoTy.unit() : MonoTy.u32();

      unify(tau, ty);
    },
    While: ({ condition, body }) => {
      inferExpr(condition, ctx, errors);
      unify(condition.ty, MonoTy.bool());
      inferExpr(body, ctx, errors);
      unify(tau, MonoTy.unit());
    },
    Error: ({ message }) => {
      errors.push(Error.Typing({ type: 'ParsingError', message }));
    },
  });

  return errors;
};

const inferPattern = (pat: Pattern, expr: Expr, ctx: TypeContext, errors: Error[]): Error[] => {
  const unify = (s: MonoTy, t: MonoTy): void => {
    errors.push(...unifyMut(s, t, ctx));
  };

  const tau = expr.ty;

  unify(Pattern.type(pat), tau);

  return errors;
};

const inferStmt = (stmt: Stmt, ctx: TypeContext, errors: Error[]): Error[] => {
  matchVariant(stmt, {
    Let: ({ name, expr, annotation }) => {
      inferExpr(expr, ctx, errors);

      annotation.do(ann => {
        errors.push(...unifyMut(expr.ty, ann, ctx));
      });

      const genTy = MonoTy.generalize(ctx.env, expr.ty);
      Env.addPoly(ctx.env, name, genTy);
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
  const unify = (s: MonoTy, t: MonoTy, context = ctx): void => {
    errors.push(...unifyMut(s, t, context));
  };

  decl.typeContext = ctx;

  matchVariant(decl, {
    Function: func => {
      const { name, typeParams, args, returnTy, body } = func;
      const bodyCtx = TypeContext.clone(ctx);
      TypeContext.declareTypeParams(bodyCtx, ...typeParams);

      args.forEach(({ annotation, name }) => {
        annotation.do(ann => {
          unify(name.ty, ann, bodyCtx);
        });

        Env.addMono(bodyCtx.env, name, name.ty);
      });

      inferExpr(body, bodyCtx, errors);

      returnTy.do(retTy => {
        unify(body.ty, retTy);
      });

      const funTy = PolyTy.instantiate(PolyTy.instantiateTyParams(typeParams, MonoTy.Fun(
        args.map(({ name: arg }) => arg.ty),
        body.ty
      )));

      unify(funTy.ty, name.ty);

      const genFunTy = MonoTy.generalize(ctx.env, funTy.ty);
      func.funTy = genFunTy;
      Env.addPoly(ctx.env, name, genFunTy);
    },
    Module: mod => {
      TypeContext.declareModule(ctx, mod);
      const modCtx = TypeContext.clone(ctx);
      TypeContext.enterModule(modCtx, mod.name);

      // declare all the functions and modules so
      // that they can be used before being defined
      for (const decl of mod.decls) {
        matchVariant(decl, {
          Function: ({ name, funTy }) => {
            Env.addPoly(modCtx.env, name, funTy);
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
    Use: ({ path, imports }) => {
      if (declare) {
        TypeContext.bringIntoScope(ctx, path, imports);
      }
    },
    Error: ({ message }) => {
      errors.push(Error.Typing({ type: 'ParsingError', message }));
    },
  });

  return errors;
};

export const infer = (prog: Prog): [Error[], TypeContext] => {
  const errors: Error[] = [];
  const ctx = TypeContext.make(prog);

  for (const decl of prog) {
    inferDecl(decl, ctx, true, errors);
  }

  return [errors, ctx];
};