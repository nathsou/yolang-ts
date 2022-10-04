import { DataType, match } from "itsamatch";
import { Error } from "../errors/errors";
import { reverse, zip } from "../utils/array";
import { Maybe, none, some } from "../utils/maybe";
import { panic } from "../utils/misc";
import { Subst } from "./subst";
import { TypeContext } from "./typeContext";
import { MonoTy } from "./types";
import { unifyMut } from "./unification";

// https://github.com/tomprimozic/type-systems/tree/master/extensible_rows

export type Row = DataType<{
  empty: {},
  extend: { field: string, ty: MonoTy, tail: MonoTy },
}, 'type'>;

export const Row = {
  empty: (): Row => ({ type: 'empty' }),
  extend: (field: string, ty: MonoTy, tail: MonoTy): Row => ({ type: 'extend', field, ty, tail }),
  fromObject: (obj: Record<string, MonoTy>): Row => {
    return Row.fromFields(Object.entries(obj));
  },
  fromFields: (fields: [string, MonoTy][], extensible = true): Row => {
    if (fields.length === 0) {
      return Row.empty();
    }

    const [[firstField, firstTy], ...tail] = reverse(fields);
    let row = Row.extend(firstField, firstTy, extensible ? MonoTy.fresh() : MonoTy.Struct(Row.empty()));

    for (const [field, ty] of tail) {
      row = Row.extend(field, ty, MonoTy.Struct(row));
    }

    return row;
  },
  fields: (row: Row, acc: [string, MonoTy][] = []): [string, MonoTy][] => {
    if (row.type === 'empty') {
      return acc;
    }

    acc.push([row.field, row.ty]);

    return Row.from(row.tail).mapWithDefault(
      tail => Row.fields(tail, acc),
      acc
    );
  },
  map: (row: Row, f: (ty: MonoTy) => MonoTy): Row => {
    if (row.type === 'empty') {
      return Row.empty();
    }

    if (row.tail.variant === 'Struct') {
      return Row.extend(row.field, f(row.ty), MonoTy.Struct(Row.map(row.tail.row, f)));
    }

    return Row.extend(row.field, row.ty, f(row.tail));
  },
  sortedFields: (row: Row): [string, MonoTy][] => {
    return Row.fields(row).sort(([a], [b]) => a.localeCompare(b));
  },
  asObject: (row: Row): { [field: string]: MonoTy } => {
    const result: { [field: string]: MonoTy } = {};

    for (const [field, ty] of Row.fields(row)) {
      result[field] = ty;
    }

    return result;
  },
  strictEq: (row1: Row, row2: Row): boolean => {
    const fields1 = Row.sortedFields(row1);
    const fields2 = Row.sortedFields(row2);

    return fields1.length === fields2.length && zip(fields1, fields2).every(([[field1, ty1], [field2, ty2]]) => {
      return field1 === field2 && MonoTy.eq(ty1, ty2);
    });
  },
  from: (ty: MonoTy): Maybe<Row> => {
    if (ty.variant === 'Struct') {
      return some(ty.row);
    }

    if (ty.variant === 'Var' && ty.value.kind === 'Link') {
      return Row.from(ty.value.to);
    }

    return none;
  },
  show: (row: Row): string => {
    const base = '{ ' + Row.fields(row).map(([name, ty]) => `${name}: ${MonoTy.show(ty)}`).join(', ') + ' }';
    if (row.type === 'extend') {
      return base + '..' + MonoTy.show(row.tail);
    }

    return base;
  },
  // https://github.com/tomprimozic/type-systems/blob/master/extensible_rows/infer.ml
  rewrite: (
    row2: MonoTy,
    field1: string,
    fieldTy1: MonoTy,
    ctx: TypeContext,
    subst: Subst | undefined,
    errors: Error[],
  ): MonoTy => {
    return match(row2, {
      Struct: ({ row: row2 }) => match(row2, {
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
            Row.rewrite(row2Tail, field1, fieldTy1, ctx, subst, errors)
          ));
        },
      }, 'type'),
      Var: v => match(v.value, {
        Unbound: () => {
          const row2Tail = MonoTy.fresh();
          const ty2 = MonoTy.Struct(Row.extend(field1, fieldTy1, row2Tail));
          MonoTy.link(v, ty2, subst);
          return row2Tail;
        },
        Link: ({ to }) => Row.rewrite(to, field1, fieldTy1, ctx, subst, errors),
      }, 'kind'),
      _: () => {
        return panic(`expected row type, got ${MonoTy.show(row2)}`);
      },
    });
  },
};
