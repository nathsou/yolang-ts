import { addSet } from "../utils/set";
import { MonoTy, PolyTy, TyVarId } from "./types";

export type Env = Record<string, PolyTy>;

export const Env = {
  empty: (): Env => ({}),
  addPoly: (env: Env, name: string, ty: PolyTy): Env => ({
    ...env,
    [name]: ty,
  }),
  addMono: (env: Env, name: string, ty: MonoTy): Env => {
    return Env.addPoly(env, name, MonoTy.toPoly(ty));
  },
  freeTypeVars: (env: Env): Set<TyVarId> => {
    const freeTypeVars = new Set<TyVarId>();

    for (const [, ty] of Object.entries(env)) {
      addSet(freeTypeVars, PolyTy.freeTypeVars(ty));
    }

    return freeTypeVars;
  },
};