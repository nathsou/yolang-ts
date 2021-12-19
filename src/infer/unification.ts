import { match as matchVariant } from "itsamatch";
import { match, __ } from "ts-pattern";
import { Row } from "./records";
import { MonoTy, TyVarId } from "./types";

export type UnificationError = string;

const occurs = (x: TyVarId, t: MonoTy): boolean =>
  matchVariant(t, {
    Var: ({ value }) => {
      if (value.kind === 'Var') {
        return value.id === x;
      } else {
        return occurs(x, value.ref);
      }
    },
    Const: t => t.args.some(a => occurs(x, a)),
    Fun: t => t.args.some(a => occurs(x, a)) || occurs(x, t.ret),
    Record: ({ row }) => Row.fields(row).some(([_, ty]) => occurs(x, ty)),
    NamedRecord: ({ fields }) => Row.fields(fields).some(([_, ty]) => occurs(x, ty)),
  });

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
      .with([{ variant: 'Var', value: { kind: 'Var' } }, __], ([s, t]) => {
        if (occurs(s.value.id, t)) {
          errors.push(`occurs check failed: ${MonoTy.show(s)}, ${MonoTy.show(t)}`);
        } else {
          /// @ts-ignore
          s.value = { kind: 'Link', ref: MonoTy.deref(t) };
        }
      })
      .with([{ variant: 'Var', value: { kind: 'Link' } }, __], ([s, t]) => {
        eqs.push([MonoTy.deref(s.value.ref), t]);
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
        { variant: 'Record', row: { type: 'empty' } },
        { variant: 'Record', row: { type: 'empty' } }
      ], () => {
        // do nothing
      })
      .with([
        { variant: 'Record', row: { type: 'extend' } },
        { variant: 'Record', row: { type: 'extend' } }
      ], ([s, t]) => {
        const isTailUnboundVar = s.row.tail.variant === 'Var' && s.row.tail.value.kind === 'Var';
        const row2Tail = rewriteRow(t, s.row.field, s.row.ty, errors);
        if (isTailUnboundVar && s.row.tail.variant === 'Var' && s.row.tail.value.kind === 'Link') {
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
    Record: ({ row: row2 }) => {
      if (row2.type === 'empty') {
        errors.push(`row does not contain field ${field1}`);
        return MonoTy.Record(Row.empty());
      }

      if (row2.field === field1) {
        errors.push(...unify(fieldTy1, row2.ty));
        return row2.tail;
      }

      return MonoTy.Record(Row.extend(
        row2.field,
        row2.ty,
        rewriteRow(row2.tail, field1, fieldTy1, errors)
      ));
    },
    Var: v => {
      if (v.value.kind === 'Link') {
        return rewriteRow(v.value.ref, field1, fieldTy1, errors);
      } else {
        const row2Tail = MonoTy.fresh();
        const ty2 = MonoTy.Record(Row.extend(field1, fieldTy1, row2Tail));
        v.value = { kind: 'Link', ref: ty2 };
        return row2Tail;
      }
    },
    _: () => {
      errors.push(`expected row type, got ${MonoTy.show(row2)}`);
      return MonoTy.Record(Row.empty());
    },
  });
};

export const unify = (s: MonoTy, t: MonoTy): UnificationError[] => {
  return unifyMany([[MonoTy.deref(s), MonoTy.deref(t)]]);
};