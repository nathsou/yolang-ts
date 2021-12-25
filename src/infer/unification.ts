import { match as matchVariant, VariantOf } from "itsamatch";
import { match, __ } from "ts-pattern";
import { panic } from "../utils/misc";
import { Result } from "../utils/result";
import { RowMono } from "./records";
import { Subst } from "./subst";
import { TypeContext } from "./typeContext";
import { MonoTy, ParameterizedTy, TyVar } from "./types";

export type UnificationError = string;

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
): UnificationError[] => {
  const errors: UnificationError[] = [];
  const pushEqs = (...newEqs: [MonoTy, MonoTy][]): void => {
    eqs.push(...newEqs.map(([s, t]) => [MonoTy.deref(s), MonoTy.deref(t)] as [MonoTy, MonoTy]));
  };

  while (eqs.length > 0) {
    const [s, t] = eqs.pop()!;

    match<[MonoTy, MonoTy]>([s, t])
      // Delete
      .when(([s, t]) => MonoTy.eq(s, t), () => { })
      // Eliminate
      .with([{ variant: 'Var', value: { kind: 'Unbound' } }, __], ([s, t]) => {
        if (MonoTy.occurs(s.value.id, t)) {
          errors.push(`occurs check failed: ${MonoTy.show(s)}, ${MonoTy.show(t)}`);
        } else {
          linkTo(s, t, subst);
        }
      })
      .with([{ variant: 'Var', value: { kind: 'Link' } }, __], ([s, t]) => {
        pushEqs([s, t]);
      })
      // Orient
      .with([__, { variant: 'Var' }], ([s, t]) => {
        pushEqs([t, s]);
      })
      // Decompose
      .with([{ variant: 'Const' }, { variant: 'Const' }], ([s, t]) => {
        if (s.name === t.name && s.args.length === t.args.length) {
          for (let i = 0; i < s.args.length; i++) {
            pushEqs([s.args[i], t.args[i]]);
          }
        } else {
          errors.push(`cannot unify ${MonoTy.show(s)} with ${MonoTy.show(t)}`);
        }
      })
      .with([{ variant: 'Fun' }, { variant: 'Fun' }], ([s, t]) => {
        if (s.args.length === t.args.length) {
          for (let i = 0; i < s.args.length; i++) {
            pushEqs([s.args[i], t.args[i]]);
          }

          pushEqs([s.ret, t.ret]);
        } else {
          errors.push(`cannot unify ${MonoTy.show(s)} with ${MonoTy.show(t)}`);
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
          errors.push('recursive row type');
          // prevent infinite loop
          return errors;
        }

        pushEqs([s.row.tail, row2Tail]);
      })
      .with([{ variant: 'Const' }, __], ([s, t]) => {
        if (s.name in ctx.typeAliases) {
          const genericTy = ctx.typeAliases[s.name].ty;
          const monoTy = ParameterizedTy.instantiate(genericTy, ctx);
          pushEqs([monoTy, t]);
        } else {
          errors.push(`No type alias named '${s.name}' found`);
        }
      })
      .with([__, { variant: 'Const' }], ([s, t]) => {
        if (t.name in ctx.typeAliases) {
          const genericTy = ctx.typeAliases[t.name].ty;
          const monoTy = ParameterizedTy.instantiate(genericTy, ctx);
          pushEqs([s, monoTy]);
        } else {
          errors.push(`No type alias named '${t.name}' found`);
        }
      })
      .otherwise(([s, t]) => {
        errors.push(`cannot unify ${MonoTy.show(s)} with ${MonoTy.show(t)}`);
      });
  }

  return errors;
};

// https://github.com/tomprimozic/type-systems/blob/master/extensible_rows/infer.ml
const rewriteRow = (
  row2: MonoTy,
  field1: string,
  fieldTy1: MonoTy,
  ctx: TypeContext,
  subst: Subst | undefined,
  errors: string[],
): MonoTy => {
  return matchVariant(row2, {
    Record: ({ row: row2 }) => matchVariant(row2, {
      empty: () => {
        errors.push(`row does not contain field ${field1}`);
        return MonoTy.Record(RowMono.empty());
      },
      extend: ({ field: field2, ty: fieldTy2, tail: row2Tail }) => {
        if (field1 === field2) {
          errors.push(...unifyMut(fieldTy1, fieldTy2, ctx));
          return row2Tail;
        }

        return MonoTy.Record(RowMono.extend(
          field2,
          fieldTy2,
          rewriteRow(row2Tail, field1, fieldTy1, ctx, subst, errors)
        ));
      },
    }, 'type'),
    Var: v => matchVariant(v.value, {
      Unbound: () => {
        const row2Tail = MonoTy.fresh();
        const ty2 = MonoTy.Record(RowMono.extend(field1, fieldTy1, row2Tail));
        linkTo(v, ty2, subst);
        return row2Tail;
      },
      Link: ({ to }) => rewriteRow(to, field1, fieldTy1, ctx, subst, errors),
    }, 'kind'),
    _: () => {
      errors.push(`expected row type, got ${MonoTy.show(row2)}`);
      return MonoTy.Record(RowMono.empty());
    },
  });
};

export const unifyMut = (s: MonoTy, t: MonoTy, ctx: TypeContext): UnificationError[] => {
  return unifyMany([[MonoTy.deref(s), MonoTy.deref(t)]], ctx);
};

export const unifyPure = (s: MonoTy, t: MonoTy, ctx: TypeContext): Result<Subst, UnificationError[]> => {
  const subst = Subst.make();
  const errors = unifyMany([[s, t]], ctx, subst);
  return Result.wrap([subst, errors]);
};