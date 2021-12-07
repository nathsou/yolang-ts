import { Maybe, none } from "./maybe";
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

export const joinWith = <T>(elems: T[], f: (elem: T) => string, sep: string): string => {
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