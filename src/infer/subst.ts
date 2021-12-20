import { joinWith } from "../utils/array";
import { MonoTy, TyVarId, showTyVarId } from "./types";

export type Subst = Map<TyVarId, MonoTy>;

export const Subst = {
  make: (): Subst => new Map(),
  from: (entries: [number, MonoTy][]): Subst => new Map(entries),
  apply: (subst: Subst, ty: MonoTy) => MonoTy.substitute(ty, subst),
  show: (subst: Subst): string => `{${joinWith([...subst.entries()], ([k, v]) => `${showTyVarId(k)} -> ${MonoTy.show(v)}`, ',')}}`,
  eq: (s1: Subst, s2: Subst): boolean => {
    if (s1.size !== s2.size) {
      return false;
    }

    for (const [k, v] of s1) {
      if (s2.has(k)) {
        return MonoTy.eq(v, s2.get(k)!);
      } else {
        return false;
      }
    }

    return true;
  },
};