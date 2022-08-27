import { DataType, match, VariantOf } from 'itsamatch';
import { Decl, Expr, Module, Pattern, Prog, Stmt } from '../ast/bitter';
import { FuncName, VarName } from '../ast/name';
import { Error } from '../errors/errors';
import { gen, groupBy, zip } from '../utils/array';
import { Either } from '../utils/either';
import { Maybe, none, some } from '../utils/maybe';
import { assert, block, proj } from '../utils/misc';
import { diffSet } from '../utils/set';
import { Env, FunDecl } from './env';
import { Row } from './structs';
import { MAX_TUPLE_INDEX, Tuple } from './tuples';
import { TypeContext } from './typeContext';
import { MonoTy, PolyTy, TypeParams } from './types';
import { unifyMut, unifyPure } from './unification';

export type TypingError = DataType<{
  UnboundVariable: { name: string },
  UnknownFunction: { name: string },
  ImmutableValue: { expr: Expr },
  CannotUseImmutableValueForMutFuncArg: { func: string, arg: string },
  UnassignableExpression: { expr: Expr },
  TupleIndexTooBig: { index: number },
  ParsingError: { message: string },
  NoOverloadMatchesCallSignature: { name: string, f: MonoTy, candidates: PolyTy[] },
  AmbiguousOverload: { name: string, funTy: string, matches: PolyTy[] },
  UndeclaredStruct: { name: string },
  MissingStructFields: { name: string, fields: string[] },
  ExtraneoussStructFields: { name: string, fields: string[] },
  MissingFuncPrototypeReturnTy: { name: string },
  ReturnUsedOutsideFunctionBody: {},
}, 'type'>;

const ASSIGNABLE_EXPRESSIONS = new Set<Expr['variant']>(['Variable', 'FieldAccess']);

const resolveOverloading = (
  name: string,
  f: MonoTy,
  candidates: FunDecl[],
  ctx: TypeContext
): Either<FunDecl, TypingError> => {
  const matches = candidates.map(func => {
    const gInst = PolyTy.instantiate(func.funTy);
    const score = unifyPure(f, gInst.ty, ctx).match({
      Ok: ({ score }) => score,
      Error: () => 0,
    });

    return { score, func };
  }).filter(({ score }) => score > 0);

  if (matches.length === 0) {
    return Either.right({ type: 'NoOverloadMatchesCallSignature', name, f, candidates: candidates.map(proj('funTy')) });
  }

  const maxScore = Math.max(...matches.map(m => m.score));
  const bestMatches = matches.filter(m => m.score === maxScore);
  if (bestMatches.length === 1) {
    return Either.left(bestMatches[0].func);
  }

  return Either.right({ type: 'AmbiguousOverload', name, funTy: MonoTy.show(f), matches: bestMatches.map(f => f.func.funTy) });
};

const inferExpr = (
  expr: Expr,
  ctx: TypeContext,
  errors: Error[]
): void => {
  const unify = (s: MonoTy, t: MonoTy): void => {
    errors.push(...unifyMut(s, t, ctx));
  };

  const { env } = ctx;
  const tau = expr.ty;

  const resolveVar = (name: VarName): Maybe<PolyTy> => {
    return Env.lookupVar(env, name.original).match({
      Some: ({ ty, name: declaredName }) => {
        if (!name.initialized) {
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

  const resolveFuncs = (name: Either<string, FuncName>): Maybe<FunDecl[]> => {
    const funcName = name.match({
      left: name => name,
      right: name => name.original,
    });

    const funcs = Env.lookupFuncs(env, funcName);

    if (funcs.length === 0) {
      errors.push(Error.Typing({ type: 'UnknownFunction', name: funcName }));
      return none;
    }

    return some(funcs);
  };

  match(expr, {
    Const: () => { },
    Variable: v => {
      resolveVar(v.name).do(ty => {
        const instTy = PolyTy.instantiate(ty);
        unify(instTy.ty, tau);
      });
    },
    NamedFuncCall: call => {
      const { name, typeParams, args } = call;

      resolveFuncs(name).do(funcs => {
        args.forEach(arg => {
          inferExpr(arg, ctx, errors);
        });

        const funcName = name.match({
          left: name => name,
          right: ({ original }) => original,
        });

        const argTys = args.map(proj('ty'));
        const actualFunTy = MonoTy.Fun(argTys, tau) as VariantOf<MonoTy, 'Fun'>;

        resolveOverloading(funcName, actualFunTy, funcs, ctx).match({
          left: resolvedFunc => {
            zip(resolvedFunc.args, args).forEach(([funcArg, receivedArg]) => {
              if (funcArg.mutable && !Expr.isMutable(receivedArg)) {
                errors.push(Error.Typing({
                  type: 'CannotUseImmutableValueForMutFuncArg',
                  func: funcName,
                  arg: funcArg.name.original
                }));
              }
            });

            call.name = Either.right(resolvedFunc.name);
            const isPoly = PolyTy.isPolymorphic(resolvedFunc.funTy);
            const [funTy, paramsInst] = block(() => {
              if (isPoly) {
                const inst = PolyTy.instantiate(resolvedFunc.funTy);
                return [inst.ty, [...inst.subst.values()]];
              } else {
                return [resolvedFunc.funTy[1], []];
              }
            });

            unify(funTy, actualFunTy);
            call.typeParams = paramsInst;

            if (paramsInst.length > 0 && paramsInst.every(MonoTy.isDetermined)) {
              resolvedFunc.instances.set(
                TypeParams.hash(paramsInst),
                paramsInst.map(MonoTy.deref)
              );
            }
          },
          right: err => {
            errors.push(Error.Typing(err));
          },
        });
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

      const retTy = lastExpr.mapWithDefault(proj('ty'), MonoTy.void());
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
      const elseTy = else_.mapWithDefault(proj('ty'), MonoTy.void());
      unify(thenTy, tau);
      unify(elseTy, tau);
    },
    Closure: ({ args, body }) => {
      const bodyCtx = TypeContext.clone(ctx);

      args.forEach(({ name, annotation }) => {
        Env.addMonoVar(bodyCtx.env, name, name.ty);
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
    Array: ({ init, elemTy }) => {
      match(init, {
        elems: ({ elems }) => {
          elems.forEach(elem => {
            inferExpr(elem, ctx, errors);
            unify(elem.ty, elemTy);
          });
        },
        fill: ({ value }) => {
          inferExpr(value, ctx, errors);
          unify(value.ty, elemTy);
        },
      });

      unify(tau, MonoTy.Array(elemTy));
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
        unify(retTy, MonoTy.void());
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
            Env.addPolyVar(bodyCtx.env, v, genTy);
          }

          inferExpr(body, bodyCtx, errors);

          unify(retTy, body.ty);
        }
      }

      unify(tau, retTy);
    },
    FieldAccess: ({ lhs, field }) => {
      const rowTail = MonoTy.fresh();
      const partialRecordTy = MonoTy.Struct(Row.extend(field, tau, rowTail));

      inferExpr(lhs, ctx, errors);
      unify(partialRecordTy, lhs.ty);
    },
    Struct: ({ name, typeParams, fields }) => {
      TypeContext.resolveTypeAlias(ctx, name).match({
        Some: ({ ty }) => {
          if (ty.variant !== 'Struct') {
            errors.push(Error.Typing({ type: 'UndeclaredStruct', name }));
          } else {
            const expectedFields = Row.fields(ty.row);
            const expectedFieldNames = new Set(expectedFields.map(proj('0')));
            const receivedFieldnames = new Set(fields.map(proj('name')));

            const missingFields = diffSet(expectedFieldNames, receivedFieldnames);
            const extraFields = diffSet(receivedFieldnames, expectedFieldNames);

            if (missingFields.size > 0) {
              errors.push(Error.Typing({ type: 'MissingStructFields', name, fields: [...missingFields] }));
            } else if (extraFields.size > 0) {
              errors.push(Error.Typing({ type: 'ExtraneoussStructFields', name, fields: [...extraFields] }));
            } else {
              fields.forEach(({ name: fieldName, value }) => {
                const [, expectedFieldTy] = expectedFields.find(([name]) => name === fieldName)!;
                inferExpr(value, ctx, errors);
                unify(value.ty, expectedFieldTy);
              });

              const expectedTy = MonoTy.Const(name, ...typeParams);
              const actualTy = MonoTy.Struct(
                Row.fromFields(fields.map(f => [f.name, f.value.ty])),
                name,
              );

              unify(expectedTy, actualTy);
              unify(tau, expectedTy);
            }
          }
        },
        None: () => {
          errors.push(Error.Typing({ type: 'UndeclaredStruct', name }));
        },
      });
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
};

const inferPattern = (pat: Pattern, expr: Expr, ctx: TypeContext, errors: Error[]): void => {
  const unify = (s: MonoTy, t: MonoTy): void => {
    errors.push(...unifyMut(s, t, ctx));
  };

  const tau = expr.ty;

  unify(Pattern.type(pat), tau);
};

const inferStmt = (stmt: Stmt, ctx: TypeContext, errors: Error[]): void => {
  match(stmt, {
    Let: ({ name, expr, annotation }) => {
      inferExpr(expr, ctx, errors);

      annotation.do(ann => {
        errors.push(...unifyMut(expr.ty, ann, ctx));
      });

      const genTy = MonoTy.generalize(ctx.env, expr.ty);
      unifyMut(name.ty, expr.ty, ctx);
      Env.addPolyVar(ctx.env, name, genTy);
    },
    Assignment: ({ lhs, rhs }) => {
      inferExpr(lhs, ctx, errors);
      inferExpr(rhs, ctx, errors);
      const lhsTy = lhs.ty;
      const rhsTy = rhs.ty;
      unifyMut(lhsTy, rhsTy, ctx);

      if (!ASSIGNABLE_EXPRESSIONS.has(lhs.variant)) {
        errors.push(Error.Typing({ type: 'UnassignableExpression', expr: lhs }));
      } else if (!Expr.isMutable(lhs)) { // mutability check
        errors.push(Error.Typing({ type: 'ImmutableValue', expr: lhs }));
      }
    },
    Expr: ({ expr }) => {
      inferExpr(expr, ctx, errors);
    },
    While: ({ condition, statements }) => {
      inferExpr(condition, ctx, errors);
      unifyMut(condition.ty, MonoTy.bool(), ctx);
      statements.forEach(s => {
        inferStmt(s, ctx, errors)
      });
    },
    Return: ({ expr }) => {
      TypeContext.getCurrentFunc(ctx).match({
        Some: ({ returnTy }) => {
          const expectedRetTy = expr.mapWithDefault(proj('ty'), MonoTy.void());
          expr.do(expr => {
            inferExpr(expr, ctx, errors);
          });
          unifyMut(expectedRetTy, returnTy, ctx);
        },
        None: () => {
          errors.push(Error.Typing({ type: 'ReturnUsedOutsideFunctionBody' }));
        },
      });
    },
    Error: ({ message }) => {
      errors.push(Error.Typing({ type: 'ParsingError', message }));
    },
  });
};

export const inferDecl = (decl: Decl, ctx: TypeContext, errors: Error[]): void => {
  const unify = (s: MonoTy, t: MonoTy, context = ctx): void => {
    errors.push(...unifyMut(s, t, context));
  };

  match(decl, {
    Function: f => {
      const { name, typeParams, args, returnTy, body } = f;

      if (body.isNone() && returnTy.isNone()) {
        errors.push(Error.Typing({ type: 'MissingFuncPrototypeReturnTy', name: name.original }));
      }

      Env.declareFunc(ctx.env, f);

      const bodyCtx = TypeContext.clone(ctx);
      TypeContext.declareTypeParams(bodyCtx, ...typeParams);

      args.forEach(({ annotation, name }) => {
        annotation.do(ann => {
          unify(name.ty, ann, bodyCtx);
        });

        Env.addMonoVar(bodyCtx.env, name, name.ty);
      });

      body.do(body => {
        bodyCtx.funcStack.push({ returnTy: body.ty });
        inferExpr(body, bodyCtx, errors);
        bodyCtx.funcStack.pop();

        returnTy.do(retTy => {
          unify(body.ty, retTy, bodyCtx);
        });

        const funTy = MonoTy.Fun(
          args.map(({ name: arg }) => arg.ty),
          body.ty
        );

        unify(funTy, name.ty);

        const genFunTy = MonoTy.generalize(ctx.env, funTy);
        f.funTy = genFunTy;
      });
    },
    TypeAlias: ({ name, typeParams, alias }) => {
      TypeContext.declareTypeAlias(ctx, name, typeParams, alias);
    },
    Import: ({ }) => { },
    Error: ({ message }) => {
      errors.push(Error.Typing({ type: 'ParsingError', message }));
    },
  });
};

const inferModule = (mod: Module, errors: Error[]): void => {
  if (!mod.typeChecked) {
    declareImports(mod, errors);

    mod.decls.forEach(decl => {
      inferDecl(decl, mod.typeContext, errors);
    });

    mod.typeChecked = true;
  }
};

const declareImports = (mod: Module, errors: Error[]): void => {
  const typeCheckMod = (path: string) => {
    const m = mod.typeContext.modules.get(path)!;
    if (!m.typeChecked) {
      inferModule(m, errors);
      m.typeChecked = true;
    }

    return m;
  };

  const declareImport = (d: VariantOf<Decl, 'Function' | 'TypeAlias'>) => {
    if (d.pub) {
      match(d, {
        Function: f => {
          Env.declareFunc(mod.typeContext.env, f);
        },
        TypeAlias: ({ name, typeParams, alias }) => {
          TypeContext.declareTypeAlias(mod.typeContext, name, typeParams, alias);
        },
      });
    }
  };

  const importsBySourceMod = groupBy([...mod.imports.keys()], name => mod.imports.get(name)!.sourceMod);
  for (const [sourceModPath, names] of Object.entries(importsBySourceMod)) {
    const sourceMod = typeCheckMod(sourceModPath);
    for (const name of names) {
      assert(sourceMod.members.has(name), 'infer: unresolved import');
      const decls = sourceMod.members.get(name)!;
      decls.forEach(declareImport);
    }
  }
};

export const infer = (prog: Prog): Error[] => {
  const errors: Error[] = [];

  // TODO: build dependency graph (in resolve?) and check for circular dependencies
  for (const mod of prog.modules.values()) {
    inferModule(mod, errors);
  }

  return errors;
};
