import { DataType, match as matchVariant } from "itsamatch";
import { joinWith, zip } from "../utils/array";
import { Maybe, none, some } from "../utils/maybe";
import { MonoTy, TyVar } from "./types";

export const MAX_TUPLE_INDEX = 1000;

export type Tuple = DataType<{
  Empty: { extension: Maybe<MonoTy> },
  Extend: { head: MonoTy, tail: Tuple },
}, 'kind'>;

export const Tuple = {
  Empty: (extension: Maybe<MonoTy> = none): Tuple => ({ kind: 'Empty', extension }),
  Extend: (head: MonoTy, tail: Tuple): Tuple => ({ kind: 'Extend', head, tail }),
  fromArray: (arr: MonoTy[], extensible = false): Tuple => {
    if (arr.length === 0) {
      return Tuple.Empty(extensible ? some(MonoTy.Var(TyVar.fresh())) : none);
    }

    const [head, ...tail] = arr;
    return Tuple.Extend(head, Tuple.fromArray(tail, extensible));
  },
  toArray: (tuple: Tuple): MonoTy[] => {
    if (tuple.kind === 'Empty') {
      return tuple.extension.match({
        Some: ty => Tuple.from(ty).match({
          Some: t => Tuple.toArray(t),
          None: () => [],
        }),
        None: () => [],
      });
    }

    return [tuple.head, ...Tuple.toArray(tuple.tail)];
  },
  iter: function* (tuple: Tuple): IterableIterator<MonoTy> {
    if (tuple.kind === 'Extend') {
      yield tuple.head;
      yield* Tuple.iter(tuple.tail);
    }
  },
  map: (tuple: Tuple, f: (t: MonoTy) => MonoTy): Tuple => {
    if (tuple.kind === 'Empty') {
      return Tuple.Empty(tuple.extension.map(f));
    }

    return Tuple.Extend(f(tuple.head), Tuple.map(tuple.tail, f));
  },
  eq: (a: Tuple, b: Tuple): boolean => {
    const as = Tuple.toArray(a);
    const bs = Tuple.toArray(b);

    if (as.length !== bs.length) {
      return false;
    }

    return zip(as, bs).every(([a, b]) => MonoTy.eq(a, b));
  },
  isExtensible: (tuple: Tuple): boolean => {
    if (tuple.kind === 'Empty') {
      return tuple.extension.isSome();
    }

    return Tuple.isExtensible(tuple.tail);
  },
  show: (tuple: Tuple) => `(${joinWith(Tuple.toArray(tuple), MonoTy.show)}${Tuple.isExtensible(tuple) ? ', ...' : ''})`,
  from: (ty: MonoTy): Maybe<Tuple> => {
    return matchVariant(ty, {
      Tuple: ({ tuple }) => some(tuple),
      Var: v => matchVariant(v.value, {
        Link: link => Tuple.from(link.to),
        Unbound: () => none,
      }, 'kind'),
      _: () => none,
    });
  },
};
