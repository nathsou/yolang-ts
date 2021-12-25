import { DataType } from "itsamatch";
import { zip } from "../utils/array";
import { Maybe, none, some } from "../utils/maybe";
import { MonoTy, ParameterizedTy } from "./types";

// https://github.com/tomprimozic/type-systems/tree/master/extensible_rows

export type Row<T> = DataType<{
  empty: {},
  extend: { field: string, ty: T, tail: T },
}, 'type'>;

type Ty<T> = {
  fresh: () => T,
  eq: (a: T, b: T) => boolean,
  Record: (row: Row<T>) => T,
  from: (ty: T) => Maybe<Row<T>>,
};

const createRow = <T>({ fresh, eq, Record, from }: Ty<T>) => {
  const Row = {
    empty: (): Row<T> => ({ type: 'empty' }),
    extend: (field: string, ty: T, tail: T): Row<T> => ({ type: 'extend', field, ty, tail }),
    fromObject: (obj: Record<string, T>): Row<T> => {
      return Row.fromFields(Object.entries(obj));
    },
    fromFields: (fields: [string, T][]): Row<T> => {
      if (fields.length === 0) {
        return Row.empty();
      }

      const [[firstField, firstTy], ...tail] = fields;
      let row = Row.extend(firstField, firstTy, fresh());

      for (let [field, ty] of tail) {
        row = Row.extend(field, ty, Record(row));
      }

      return row;
    },
    fields: (row: Row<T>, acc: [string, T][] = []): [string, T][] => {
      if (row.type === 'empty') {
        return acc;
      }

      acc.push([row.field, row.ty]);

      return from(row.tail).mapWithDefault(
        tail => Row.fields(tail, acc),
        acc
      );
    },
    sortedFields: (row: Row<T>): [string, T][] => {
      return Row.fields(row).sort(([a], [b]) => a.localeCompare(b));
    },
    asObject: (row: Row<T>): { [field: string]: T } => {
      const result: { [field: string]: T } = {};

      for (const [field, ty] of Row.fields(row)) {
        result[field] = ty;
      }

      return result;
    },
    strictEq: (row1: Row<T>, row2: Row<T>): boolean => {
      const fields1 = Row.sortedFields(row1);
      const fields2 = Row.sortedFields(row2);

      return fields1.length === fields2.length && zip(fields1, fields2).every(([[field1, ty1], [field2, ty2]]) => {
        return field1 === field2 && eq(ty1, ty2);
      });
    },
    from,
  };

  return Row;
};

export type RowMono = Row<MonoTy>;

export const RowMono = createRow<MonoTy>({
  fresh: () => MonoTy.fresh(),
  eq: (a, b) => MonoTy.eq(a, b),
  Record: row => MonoTy.Record(row),
  from: (ty: MonoTy): Maybe<Row<MonoTy>> => {
    if (ty.variant === 'Record') {
      return some(ty.row);
    }

    if (ty.variant === 'Var' && ty.value.kind === 'Link') {
      return RowMono.from(ty.value.to);
    }

    return none;
  },
});

export type RowGeneric = Row<ParameterizedTy>;

export const RowGeneric = createRow<ParameterizedTy>({
  fresh: () => ParameterizedTy.fresh(),
  eq: (a, b) => ParameterizedTy.eq(a, b),
  Record: row => ParameterizedTy.Record(row),
  from: (ty: ParameterizedTy): Maybe<Row<ParameterizedTy>> => {
    if (ty.variant === 'Record') {
      return some(ty.row);
    }

    return none;
  },
});