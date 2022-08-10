import { DataType, match as matchVariant, VariantOf } from "itsamatch";
import { match, P } from "ts-pattern";
import { Error } from "../errors/errors";
import { zip } from "../utils/array";
import { Maybe, none } from "../utils/maybe";
import { panic, proj } from "../utils/misc";
import { Result } from "../utils/result";
import { Row } from "./structs";
import { Subst } from "./subst";
import { Tuple } from "./tuples";
import { TypeContext } from "./typeContext";
import { MonoTy, TypeParam, TyVar } from "./types";

export type UnificationError = DataType<{
  RecursiveType: { s: MonoTy, t: MonoTy },
  Ununifiable: { s: MonoTy, t: MonoTy },
  UnknownStructField: { row: Row, field: string },
  DifferentLengthTuples: { s: Tuple, t: Tuple },
  CouldNotResolveType: { ty: MonoTy },
}, 'type'>;

const linkTo = (v: VariantOf<MonoTy, 'Var'>, to: MonoTy, subst?: Subst) => {
  if (subst) {
    if (v.value.kind === 'Link') {
      return panic('cannot link to a bound type variable');
    }

    subst.set(v.value.id, MonoTy.deref(to));
  } else {
    const link: TyVar = { kind: 'Link', to: MonoTy.deref(to) };
    /// @ts-ignore
    v.value = link;
  }
};

const UNIFICATION_SCORE = {
  exactMatch: 3,
  assignableRecord: 2,
  instanciation: 1,
};

// if subst is present, the unification does not mutate type variables
const unifyMany = (
  eqs: [MonoTy, MonoTy][],
  ctx: TypeContext,
  subst?: Subst
): { errors: Error[], score: number } => {
  const errors: Error[] = [];
  let score = 0;
  const pushEqs = (...newEqs: [MonoTy, MonoTy][]): void => {
    eqs.push(...newEqs.map(([s, t]) => [MonoTy.deref(s), MonoTy.deref(t)] as [MonoTy, MonoTy]));
  };

  const instantiateGenericTyConst = (c: VariantOf<MonoTy, 'Const'>): Maybe<MonoTy> => {
    const resolveTypeAlias = (ty: VariantOf<MonoTy, 'Const'>): Maybe<{ ty: MonoTy, params: TypeParam[] }> => {
      if (MonoTy.isPrimitive(ty)) {
        return none;
      }

      const res = TypeContext.findTypeAlias(ctx, ty.path, ty.name).map(([ty, params]) => ({ ty, params }));
      if (res.isNone()) {
        errors.push(Error.Unification({ type: 'CouldNotResolveType', ty }));
      }

      return res;
    };

    // Type parameters are parsed as Const types
    // if the type parameters environment contains this Const name
    // then interpret it as a type parameter
    const typeParam = TypeContext.resolveTypeParam(ctx, c.name);
    const typeAlias = () => resolveTypeAlias(c).map(({ ty: genericTy, params }) => {
      const subst = new Map<string, MonoTy>(zip(params.map(proj('name')), c.args));
      return MonoTy.substituteTyParams(genericTy, subst);
    });

    return typeParam.or(typeAlias);
  };

  while (eqs.length > 0) {
    const [s, t] = eqs.pop()!;

    match<[MonoTy, MonoTy]>([s, t])
      .with([{ variant: 'Const' }, { variant: 'Const' }], ([s, t]) => {
        let pushed = false;
        instantiateGenericTyConst(s).do(s => {
          pushed = true;
          pushEqs([s, t]);
        });

        if (!pushed) {
          instantiateGenericTyConst(t).do(t => {
            pushed = true;
            pushEqs([s, t]);
          });
        }

        if (!pushed) {
          if (s.name === t.name && s.args.length === t.args.length) {
            for (let i = 0; i < s.args.length; i++) {
              pushEqs([s.args[i], t.args[i]]);
            }

            if (s.args.length === 0) {
              score += UNIFICATION_SCORE.exactMatch;
            }
          } else {
            errors.push(Error.Unification({ type: 'Ununifiable', s, t }));
          }
        }
      })
      .when(([s]) => s.variant === 'Const' && !MonoTy.isPrimitive(s), ([s, t]) => {
        instantiateGenericTyConst(s as VariantOf<MonoTy, 'Const'>).do(s => {
          pushEqs([s, t]);
        });
      })
      .when(([, t]) => t.variant === 'Const' && !MonoTy.isPrimitive(t), ([s, t]) => {
        instantiateGenericTyConst(t as VariantOf<MonoTy, 'Const'>).do(t => {
          pushEqs([s, t]);
        });
      })
      // Delete
      .when(([s, t]) => MonoTy.eq(s, t), () => {
        score += UNIFICATION_SCORE.exactMatch;
      })
      // Eliminate
      .with([{ variant: 'Var', value: { kind: 'Unbound' } }, P._], ([s, t]) => {
        if (MonoTy.occurs(s.value.id, t)) {
          errors.push(Error.Unification({ type: 'RecursiveType', s, t }));
        } else {
          linkTo(s, t, subst);
          score += UNIFICATION_SCORE.instanciation;
        }
      })
      .with([{ variant: 'Var', value: { kind: 'Unbound' } }, P._], ([s, t]) => {
        if (MonoTy.occurs(s.value.id, t)) {
          errors.push(Error.Unification({ type: 'RecursiveType', s, t }));
        } else {
          linkTo(s, t, subst);
          score += UNIFICATION_SCORE.instanciation;
        }
      })
      // Orient
      .with([P._, { variant: 'Var' }], ([s, t]) => {
        pushEqs([t, s]);
      })
      .with([{ variant: 'Var', value: { kind: 'Link' } }, P._], ([s, t]) => {
        pushEqs([s, t]);
      })
      .with([{ variant: 'Fun' }, { variant: 'Fun' }], ([s, t]) => {
        if (s.args.length === t.args.length) {
          for (let i = 0; i < s.args.length; i++) {
            pushEqs([s.args[i], t.args[i]]);
          }

          pushEqs([s.ret, t.ret]);
        } else {
          errors.push(Error.Unification({ type: 'Ununifiable', s, t }));
        }
      })
      .with([
        { variant: 'Struct', row: { type: 'empty' } },
        { variant: 'Struct', row: { type: 'empty' } }
      ], () => {
        // the records have the same fields
        score += UNIFICATION_SCORE.exactMatch;
      })
      .with([
        { variant: 'Struct', row: { type: 'empty' } },
        { variant: 'Struct', row: { type: 'extend' } }
      ], () => {
        // the lhs record is assignable to the rhs record
        score += UNIFICATION_SCORE.assignableRecord;
      })
      .with([
        { variant: 'Struct', row: { type: 'extend' } },
        { variant: 'Struct', row: { type: 'extend' } }
      ], ([s, t]) => {
        const isTailUnboundVar = s.row.tail.variant === 'Var' && s.row.tail.value.kind === 'Unbound';
        const row2Tail = rewriteRow(t, s.row.field, s.row.ty, ctx, subst, errors);
        if (isTailUnboundVar && s.row.tail.variant === 'Var' && s.row.tail.value.kind === 'Link') {
          errors.push(Error.Unification({ type: 'RecursiveType', s, t }));
          // prevent infinite loop
          return errors;
        }

        pushEqs([s.row.tail, row2Tail]);
      })
      .with([{ variant: 'Tuple' }, { variant: 'Tuple' }], ([s, t]) => {
        const unifScore = unifyTuples(s.tuple, t.tuple, ctx, subst, errors);
        if (unifScore === 0) {
          errors.push(Error.Unification({ type: 'DifferentLengthTuples', s: s.tuple, t: t.tuple }));
        }
        score += unifScore;
      })
      .with([{ variant: 'Param' }, { variant: 'Param' }], ([s, t]) => {
        if (s.name !== t.name) {
          t.name = s.name;
        }
      })
      .otherwise(([s, t]) => {
        errors.push(Error.Unification({ type: 'Ununifiable', s, t }));
      });
  }

  return { errors, score };
};

const unifyTuples = (
  a: Tuple,
  b: Tuple,
  ctx: TypeContext,
  subst: Subst | undefined,
  errors: Error[]
): number => {
  if (a.kind === 'EmptyTuple' && a.extension.isSome()) {
    const res = unifyMany([[a.extension.unwrap(), MonoTy.Tuple(b)]], ctx, subst);
    errors.push(...res.errors);
    return res.score;
  }

  if (b.kind === 'EmptyTuple' && b.extension.isSome()) {
    const res = unifyMany([[MonoTy.Tuple(a), b.extension.unwrap()]], ctx, subst);
    errors.push(...res.errors);
    return res.score;
  }

  if (a.kind === 'EmptyTuple' && b.kind === 'EmptyTuple') {
    return UNIFICATION_SCORE.exactMatch;
  }

  if (a.kind === 'ExtendTuple' && b.kind === 'ExtendTuple') {
    const res = unifyMany([[a.head, b.head]], ctx, subst);
    errors.push(...res.errors);
    return res.score + unifyTuples(a.tail, b.tail, ctx, subst, errors);
  }

  return 0;
};

// https://github.com/tomprimozic/type-systems/blob/master/extensible_rows/infer.ml
const rewriteRow = (
  row2: MonoTy,
  field1: string,
  fieldTy1: MonoTy,
  ctx: TypeContext,
  subst: Subst | undefined,
  errors: Error[],
): MonoTy => {
  return matchVariant(row2, {
    Struct: ({ row: row2 }) => matchVariant(row2, {
      empty: () => {
        errors.push(Error.Unification({ type: 'UnknownStructField', field: field1, row: row2 }));
        return MonoTy.Struct(Row.empty());
      },
      extend: ({ field: field2, ty: fieldTy2, tail: row2Tail }) => {
        if (field1 === field2) {
          errors.push(...unifyMut(fieldTy1, fieldTy2, ctx));
          return row2Tail;
        }

        return MonoTy.Struct(Row.extend(
          field2,
          fieldTy2,
          rewriteRow(row2Tail, field1, fieldTy1, ctx, subst, errors)
        ));
      },
    }, 'type'),
    Var: v => matchVariant(v.value, {
      Unbound: () => {
        const row2Tail = MonoTy.fresh();
        const ty2 = MonoTy.Struct(Row.extend(field1, fieldTy1, row2Tail));
        linkTo(v, ty2, subst);
        return row2Tail;
      },
      Link: ({ to }) => rewriteRow(to, field1, fieldTy1, ctx, subst, errors),
    }, 'kind'),
    _: () => {
      return panic(`expected row type, got ${MonoTy.show(row2)}`);
    },
  });
};

export const unifyMut = (s: MonoTy, t: MonoTy, ctx: TypeContext): Error[] => {
  return unifyMany([[MonoTy.deref(s), MonoTy.deref(t)]], ctx).errors;
};

export const unifyPure = (s: MonoTy, t: MonoTy, ctx: TypeContext): Result<{ subst: Subst, score: number }, Error[]> => {
  const subst = Subst.make();
  const { errors, score } = unifyMany([[s, t]], ctx, subst);
  return Result.wrap([{ subst, score }, errors]);
};