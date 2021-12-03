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