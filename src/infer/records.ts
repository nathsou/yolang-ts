import { zip } from "../utils/array";
import { Maybe, none, some } from "../utils/maybe";
import { MonoTy } from "./types";

// https://github.com/tomprimozic/type-systems/tree/master/extensible_rows

export type Row =
  { type: 'empty' } |
  { type: 'extend', field: string, ty: MonoTy, tail: MonoTy };

export const Row = {
  empty: (): Row => ({ type: 'empty' }),
  extend: (field: string, ty: MonoTy, tail: MonoTy): Row => ({ type: 'extend', field, ty, tail }),
  fromObject: (obj: Record<string, MonoTy>): Row => {
    return Row.fromFields(Object.entries(obj));
  },
  fromFields: (fields: [string, MonoTy][]): Row => {
    if (fields.length === 0) {
      return Row.empty();
    }

    const [[firstField, firstTy], ...tail] = fields;
    let row = Row.extend(firstField, firstTy, MonoTy.fresh());

    for (let [field, ty] of tail) {
      row = Row.extend(field, ty, MonoTy.Record(row));
    }

    return row;
  },
  fromMonoTy: (ty: MonoTy): Maybe<Row> => {
    if (ty.variant === 'Record') {
      return some(ty.row);
    }

    if (ty.variant == 'Var' && ty.value.kind === 'Link') {
      return Row.fromMonoTy(ty.value.to);
    }

    return none;
  },
  fields: (row: Row, acc: [string, MonoTy][] = []): [string, MonoTy][] => {
    if (row.type === 'empty') {
      return acc;
    }

    acc.push([row.field, row.ty]);

    return Row.fromMonoTy(row.tail).mapWithDefault(
      tail => Row.fields(tail, acc),
      acc
    );
  },
  asObject: (row: Row): { [field: string]: MonoTy } => {
    let result: { [field: string]: MonoTy } = {};

    for (let [field, ty] of Row.fields(row)) {
      result[field] = ty;
    }

    return result;
  },
  strictEq: (row1: Row, row2: Row): boolean => {
    return zip(Row.fields(row1), Row.fields(row2)).every(([[field1, ty1], [field2, ty2]]) => {
      return field1 === field2 && MonoTy.eq(ty1, ty2);
    });
  },
};