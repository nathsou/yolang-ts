import { DataType } from "itsamatch";
import { reverse, zip } from "../utils/array";
import { Maybe, none, some } from "../utils/maybe";
import { MonoTy } from "./types";

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
  show: (row: Row): string => '{ ' + Row.fields(row).map(([name, ty]) => `${name}: ${MonoTy.show(ty)}`).join(', ') + ' }',
};