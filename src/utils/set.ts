import { boolean } from "fast-check";

export const keepSet = <T>(set: Set<T>, predicate: (value: T) => boolean): Set<T> => {
  const kept = new Set<T>();

  for (const value of set) {
    if (predicate(value)) {
      kept.add(value);
    }
  }

  return kept;
};

export const diffSet = <T>(a: Set<T>, b: Set<T>): Set<T> => {
  return keepSet(a, value => !b.has(value));
};

export const addSet = <T>(set: Set<T>, values: Iterable<T>): void => {
  for (const value of values) {
    set.add(value);
  }
};

export const sameSet = <T>(a: Set<T>, b: Set<T>): boolean => {
  if (a.size !== b.size) {
    return false;
  }

  for (const elem of a) {
    if (!b.has(elem)) {
      return false;
    }
  }

  return true;
};
