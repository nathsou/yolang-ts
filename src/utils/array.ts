import { Maybe, none, some } from "./maybe";
import { error, Result } from "./result";

export const firstSomeBy = <T, U>(elems: T[], f: (elem: T) => Maybe<U>): Maybe<U> => {
  for (const elem of elems) {
    const mapped = f(elem);
    if (mapped.isSome()) {
      return mapped;
    }
  }

  return none;
};

export const firstOkBy = <T, E, U>(elems: T[], f: (elem: T) => Result<U, E>, err: E): Result<U, E> => {
  for (const elem of elems) {
    const mapped = f(elem);
    if (mapped.isOk()) {
      return mapped;
    }
  }

  return error(err);
};

export const findRev = <T>(elems: T[], pred: (elem: T) => boolean): Maybe<T> => {
  for (let i = elems.length - 1; i >= 0; i--) {
    if (pred(elems[i])) {
      return some(elems[i]);
    }
  }

  return none;
};

export const joinWith = <T>(elems: T[], f: (elem: T) => string, sep = ', '): string => {
  return elems.map(f).join(sep);
};

export const gen = <T>(n: number, f: (n: number) => T): T[] => {
  const result: T[] = [];

  for (let i = 0; i < n; i++) {
    result.push(f(i));
  }

  return result;
};

export const zip = <A, B>(as: A[], bs: B[]): [A, B][] => {
  const zipped: [A, B][] = [];
  const len = Math.min(as.length, bs.length);

  for (let i = 0; i < len; i++) {
    zipped.push([as[i], bs[i]]);
  }

  return zipped;
};

export function* indexed<T>(elems: Iterable<T>): IterableIterator<[T, number]> {
  let i = 0;
  for (const elem of elems) {
    yield [elem, i++];
  }
}

export const takeWhile = <T>(elems: Iterable<T>, pred: (elem: T) => boolean): T[] => {
  const result: T[] = [];

  for (const elem of elems) {
    if (!pred(elem)) {
      break;
    }

    result.push(elem);
  }

  return result;
};

export const uniq = <T>(elems: T[]): T[] => {
  return [...new Set(elems)];
};

export const last = <T>(elems: T[]): T => {
  return elems[elems.length - 1];
};

export const filterMap = <T, U>(elems: T[], f: (elem: T) => Maybe<U>): U[] => {
  const result: U[] = [];

  for (const elem of elems) {
    const mapped = f(elem);
    if (mapped.isSome()) {
      result.push(mapped.unwrap());
    }
  }

  return result;
};

export const compact = <T>(values: (T | undefined)[]): T[] => {
  return values.filter(v => v !== undefined) as T[];
};

export const decons = <T>(vals: T[]): [T, T[]] => {
  return [vals[0], vals.slice(1)];
};

export const deconsLast = <T>(vals: T[]): [T[], T] => {
  return [vals.slice(0, -1), last(vals)];
};