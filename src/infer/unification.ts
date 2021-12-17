import { match as matchVariant } from "itsamatch";
import { match, __ } from "ts-pattern";
import { Row } from "./records";
import { MonoTy, TyVarId } from "./types";

export type UnificationError = string;

const occurs = (x: TyVarId, t: MonoTy): boolean => {
  return matchVariant(t, {
    TyVar: ({ value }) => {
      if (value.kind === 'Var') {
        return value.id === x;
      } else {
        return occurs(x, value.ref);
      }
    },
    TyConst: t => t.args.some(a => occurs(x, a)),
    TyFun: t => t.args.some(a => occurs(x, a)) || occurs(x, t.ret),
    TyRecord: ({ row }) => Row.fields(row).some(([_, ty]) => occurs(x, ty)),
  });
};

const unifyMany = (eqs: [MonoTy, MonoTy][]): UnificationError[] => {
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
      .with([{ variant: 'TyVar', value: { kind: 'Var' } }, __], ([s, t]) => {
        if (occurs(s.value.id, t)) {
          errors.push(`occurs check failed: ${MonoTy.show(s)}, ${MonoTy.show(t)}`);
        } else {
          /// @ts-ignore
          s.value = { kind: 'Link', ref: MonoTy.deref(t) };
        }
      })
      .with([{ variant: 'TyVar', value: { kind: 'Link' } }, __], ([s, t]) => {
        eqs.push([MonoTy.deref(s.value.ref), t]);
        pushEqs([s, t]);
      })
      // Orient
      .with([__, { variant: 'TyVar' }], ([s, t]) => {
        pushEqs([t, s]);
      })
      // Decompose
      .with([{ variant: 'TyConst' }, { variant: 'TyConst' }], ([s, t]) => {
        if (s.name === t.name && s.args.length === t.args.length) {
          for (let i = 0; i < s.args.length; i++) {
            pushEqs([s.args[i], t.args[i]]);
          }
        } else {
          errors.push(`cannot unify ${MonoTy.show(s)} with ${MonoTy.show(t)}`);
        }
      })
      .with([{ variant: 'TyFun' }, { variant: 'TyFun' }], ([s, t]) => {
        if (s.args.length === t.args.length) {
          for (let i = 0; i < s.args.length; i++) {
            pushEqs([s.args[i], t.args[i]]);
          }

          pushEqs([s.ret, t.ret]);
        } else {
          const msg = (() => {
            const len = s.args.length;
            if (len === 0) {
              return `expected no arguments, received ${t.args.length}`;
            }

            return `expected ${len} argument${len === 1 ? '' : 's'}, received ${t.args.length}`;
          })();

          errors.push(msg);
        }
      })
      .with([
        { variant: 'TyRecord', row: { type: 'empty' } },
        { variant: 'TyRecord', row: { type: 'empty' } }
      ], () => {
        // do nothing
      })
      .with([
        { variant: 'TyRecord', row: { type: 'extend' } },
        { variant: 'TyRecord', row: { type: 'extend' } }
      ], ([s, t]) => {
        const isTailUnboundVar = s.row.tail.variant === 'TyVar' && s.row.tail.value.kind === 'Var';
        const row2Tail = rewriteRow(t, s.row.field, s.row.ty, errors);
        if (isTailUnboundVar && s.row.tail.variant === 'TyVar' && s.row.tail.value.kind === 'Link') {
          errors.push('recursive row type');
          // prevent infinite loop
          return errors;
        }

        pushEqs([s.row.tail, row2Tail]);
      })
      .otherwise(([s, t]) => {
        errors.push(`cannot unify ${MonoTy.show(s)} with ${MonoTy.show(t)}`);
      });
  }

  return errors;
};

// https://github.com/tomprimozic/type-systems/blob/master/extensible_rows/infer.ml
export const rewriteRow = (row2: MonoTy, field1: string, fieldTy1: MonoTy, errors: string[]): MonoTy => {
  return matchVariant(row2, {
    TyRecord: ({ row: row2 }) => {
      if (row2.type === 'empty') {
        errors.push(`row does not contain field ${field1}`);
        return MonoTy.TyRecord(Row.empty());
      }

      if (row2.field === field1) {
        errors.push(...unify(fieldTy1, row2.ty));
        return row2.tail;
      }

      return MonoTy.TyRecord(Row.extend(
        row2.field,
        row2.ty,
        rewriteRow(row2.tail, field1, fieldTy1, errors)
      ));
    },
    TyVar: v => {
      if (v.value.kind === 'Link') {
        return rewriteRow(v.value.ref, field1, fieldTy1, errors);
      } else {
        const row2Tail = MonoTy.fresh();
        const ty2 = MonoTy.TyRecord(Row.extend(field1, fieldTy1, row2Tail));
        v.value = { kind: 'Link', ref: ty2 };
        return row2Tail;
      }
    },
    _: () => {
      errors.push(`expected row type, got ${MonoTy.show(row2)}`);
      return MonoTy.TyRecord(Row.empty());
    },
  });
};

export const unify = (s: MonoTy, t: MonoTy): UnificationError[] => {
  return unifyMany([[MonoTy.deref(s), MonoTy.deref(t)]]);
};