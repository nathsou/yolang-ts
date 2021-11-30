import { Maybe, None } from "./maybe";

export const firstSomeBy = <T, U>(elems: T[], f: (elem: T) => Maybe<U>): Maybe<U> => {
  for (const elem of elems) {
    const mapped = f(elem);
    if (mapped.isSome()) {
      return mapped;
    }
  }

  return None;
};