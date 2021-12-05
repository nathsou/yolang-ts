
const keepSet = <T>(set: Set<T>, predicate: (value: T) => boolean): Set<T> => {
  const kept = new Set<T>();

  for (const value of set) {
    if (predicate(value)) {
      kept.add(value);
    }
  }

  return kept;
};

const diffSet = <T>(a: Set<T>, b: Set<T>): Set<T> => {
  return keepSet(a, value => !b.has(value));
};