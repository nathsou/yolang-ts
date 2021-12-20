import { Maybe, none, some } from "./maybe";

export type Slice<T> = {
  readonly elems: T[],
  start: number,
  end: number,
};

export const Slice = {
  from: <T>(elems: T[]): Slice<T> => ({ elems, start: 0, end: elems.length }),
  clone: <T>(slice: Slice<T>): Slice<T> => ({ ...slice }),
  head: <T>(slice: Slice<T>): Maybe<T> => !Slice.isEmpty(slice) ? some(slice.elems[slice.start]) : none,
  tail: <T>(slice: Slice<T>): Slice<T> => Slice.step(slice, 1),
  step: <T>(slice: Slice<T>, stepCount = 1): Slice<T> => ({
    elems: slice.elems,
    start: slice.start + stepCount,
    end: slice.end
  }),
  length: <T>(slice: Slice<T>): number => slice.end - slice.start,
  isEmpty: <T>(slice: Slice<T>): boolean => Slice.length(slice) === 0,
  at: <T>(slice: Slice<T>, index: number): Maybe<T> => {
    if (index < 0 || index >= Slice.length(slice)) {
      return none;
    }

    return some(slice.elems[slice.start + index]);
  },
  stepMut: <T>(slice: Slice<T>, stepCount = 1): void => {
    slice.start += stepCount;
  },
};