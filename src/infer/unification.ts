import { DataType, match as matchVariant, VariantOf } from "itsamatch";
import { match, __ } from "ts-pattern";
import { Error } from "../errors/errors";
import { zip } from "../utils/array";
import { Maybe, none, some } from "../utils/maybe";
import { panic } from "../utils/misc";
import { Result } from "../utils/result";
import { Row } from "./records";
import { Subst } from "./subst";
import { Tuple } from "./tuples";
import { TypeContext } from "./typeContext";
import { MonoTy, TypeParams, TyVar } from "./types";

export type UnificationError = DataType<{
  RecursiveType: { s: MonoTy, t: MonoTy },
  Ununifiable: { s: MonoTy, t: MonoTy },
  UnknownRecordField: { row: Row, field: string },
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

// if subst is present, the unification does not mutate type variables
const unifyMany = (
  eqs: [MonoTy, MonoTy][],
  ctx: TypeContext,
  subst?: Subst
): Error[] => {
  const errors: Error[] = [];
  const pushEqs = (...newEqs: [MonoTy, MonoTy][]): void => {
    eqs.push(...newEqs.map(([s, t]) => [MonoTy.deref(s), MonoTy.deref(t)] as [MonoTy, MonoTy]));
  };

  const resolveTypeAlias = (ty: VariantOf<MonoTy, 'Const'>): Maybe<{ ty: MonoTy, params: TypeParams }> => {
    if (MonoTy.isPrimitive(ty)) {
      return none;
    }

    const res = TypeContext.findTypeAlias(ctx, ty.path, ty.name).map(([ty, params]) => ({ ty, params }));
    if (res.isNone()) {
      errors.push(Error.Unification({ type: 'CouldNotResolveType', ty }));
    }

    return res;
  };

  const instantiateGenericTyConst = (c: VariantOf<MonoTy, 'Const'>): Maybe<MonoTy> => {
    // Type parameters are parsed as Const types
    // if the type parameters environment contains this Const name
    // then interpret it as a type parameter
    const typeParam = TypeContext.resolveTypeParam(ctx, c.name);
    const typeAlias = () => resolveTypeAlias(c).map(({ ty: genericTy, params }) => {
      const subst = new Map<string, MonoTy>(zip(params, c.args));
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
      .when(([s, t]) => MonoTy.eq(s, t), () => { })
      // Eliminate
      .with([{ variant: 'Var', value: { kind: 'Unbound' } }, __], ([s, t]) => {
        if (MonoTy.occurs(s.value.id, t)) {
          errors.push(Error.Unification({ type: 'RecursiveType', s, t }));
        } else {
          linkTo(s, t, subst);
        }
      })
      // Orient
      .with([__, { variant: 'Var' }], ([s, t]) => {
        pushEqs([t, s]);
      })
      .with([{ variant: 'Var', value: { kind: 'Link' } }, __], ([s, t]) => {
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
        { variant: 'Record', row: { type: 'empty' } },
        { variant: 'Record', row: { type: 'empty' } }
      ], () => {
        // do nothing
      })
      .with([
        { variant: 'Record', row: { type: 'extend' } },
        { variant: 'Record', row: { type: 'extend' } }
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
        if (!unifyTuples(s.tuple, t.tuple, ctx, subst, errors)) {
          errors.push(Error.Unification({ type: 'DifferentLengthTuples', s: s.tuple, t: t.tuple }));
        }
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

  return errors;
};

const unifyTuples = (
  a: Tuple,
  b: Tuple,
  ctx: TypeContext,
  subst: Subst | undefined,
  errors: Error[]
): boolean => {
  if (a.kind === 'EmptyTuple' && a.extension.isSome()) {
    errors.push(...unifyMany([[a.extension.unwrap(), MonoTy.Tuple(b)]], ctx, subst));
    return true;
  }

  if (b.kind === 'EmptyTuple' && b.extension.isSome()) {
    errors.push(...unifyMany([[MonoTy.Tuple(a), b.extension.unwrap()]], ctx, subst));
    return true;
  }

  if (a.kind === 'EmptyTuple' && b.kind === 'EmptyTuple') {
    return true;
  }

  if (a.kind === 'ExtendTuple' && b.kind === 'ExtendTuple') {
    errors.push(...unifyMany([[a.head, b.head]], ctx, subst));
    unifyTuples(a.tail, b.tail, ctx, subst, errors);
    return true;
  }

  return false;
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
    Record: ({ row: row2 }) => matchVariant(row2, {
      empty: () => {
        errors.push(Error.Unification({ type: 'UnknownRecordField', field: field1, row: row2 }));
        return MonoTy.Record(Row.empty());
      },
      extend: ({ field: field2, ty: fieldTy2, tail: row2Tail }) => {
        if (field1 === field2) {
          errors.push(...unifyMut(fieldTy1, fieldTy2, ctx));
          return row2Tail;
        }

        return MonoTy.Record(Row.extend(
          field2,
          fieldTy2,
          rewriteRow(row2Tail, field1, fieldTy1, ctx, subst, errors)
        ));
      },
    }, 'type'),
    Var: v => matchVariant(v.value, {
      Unbound: () => {
        const row2Tail = MonoTy.fresh();
        const ty2 = MonoTy.Record(Row.extend(field1, fieldTy1, row2Tail));
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
  return unifyMany([[MonoTy.deref(s), MonoTy.deref(t)]], ctx);
};

export const unifyPure = (s: MonoTy, t: MonoTy, ctx: TypeContext): Result<Subst, Error[]> => {
  const subst = Subst.make();
  const errors = unifyMany([[s, t]], ctx, subst);
  return Result.wrap([subst, errors]);
};