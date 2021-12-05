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
      for (const tyVar of PolyTy.freeTypeVars(ty)) {
        freeTypeVars.add(tyVar);
      }
    }

    return freeTypeVars;
  },
};