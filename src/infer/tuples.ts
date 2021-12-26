import { DataType, match as matchVariant } from "itsamatch";
import { joinWith } from "../utils/array";
import { Maybe, none, some } from "../utils/maybe";
import { MonoTy, ParameterizedTy, TyVar } from "./types";

export type Tuple<T> = DataType<{
  EmptyTuple: { extension: Maybe<T> },
  ExtendTuple: { head: T, tail: Tuple<T> },
}, 'kind'>;

export type TupleMono = Tuple<MonoTy>;
export type TupleGeneric = Tuple<ParameterizedTy>;

const createTuple = <T>(from: (data: T) => Maybe<Tuple<T>>) => {
  const Tuple = {
    Empty: <U>(extension: Maybe<U> = none): Tuple<U> => ({ kind: 'EmptyTuple', extension }),
    Extend: <U>(head: U, tail: Tuple<U>): Tuple<U> => ({ kind: 'ExtendTuple', head, tail }),
    fromArray: (arr: T[], extensible = false): Tuple<T> => {
      if (arr.length === 0) {
        return Tuple.Empty(extensible ? some(MonoTy.Var(TyVar.fresh())) : none);
      }

      const [head, ...tail] = arr;
      return Tuple.Extend(head, Tuple.fromArray(tail, extensible));
    },
    toArray: (tuple: Tuple<T>): T[] => {
      if (tuple.kind === 'EmptyTuple') {
        return tuple.extension.match({
          Some: ty => from(ty).match({
            Some: t => Tuple.toArray(t),
            None: () => [],
          }),
          None: () => [],
        });
      }

      return [tuple.head, ...Tuple.toArray(tuple.tail)];
    },
    iter: function* (tuple: Tuple<T>): IterableIterator<T> {
      if (tuple.kind === 'ExtendTuple') {
        yield tuple.head;
        yield* Tuple.iter(tuple.tail);
      }
    },
    map: <U>(tuple: Tuple<T>, f: (t: T) => U): Tuple<U> => {
      if (tuple.kind === 'EmptyTuple') {
        return Tuple.Empty();
      }

      return Tuple.Extend(f(tuple.head), Tuple.map(tuple.tail, f));
    },
    show: (tuple: Tuple<T>, showData: (data: T) => string) => `(${joinWith(Tuple.toArray(tuple), showData, ', ')})`,
    from,
  };

  return Tuple;
};

export const TupleMono = createTuple<MonoTy>((ty: MonoTy): Maybe<Tuple<MonoTy>> => {
  return matchVariant(ty, {
    Tuple: ({ tuple }) => some(tuple),
    Var: v => matchVariant(v.value, {
      Link: link => TupleMono.from(link.to),
      Unbound: () => none,
    }, 'kind'),
    _: () => none,
  });
});

export const TupleGeneric = createTuple<ParameterizedTy>((ty: ParameterizedTy): Maybe<Tuple<ParameterizedTy>> => {
  return matchVariant(ty, {
    Tuple: ({ tuple }) => some(tuple),
    _: () => none,
  });
});