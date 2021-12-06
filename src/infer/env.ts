import { joinWith } from "../utils/array";
import { Maybe, none, some } from "../utils/maybe";
import { addSet } from "../utils/set";
import { MonoTy, PolyTy, TyVarId } from "./types";

export type Env = Record<string, PolyTy>;

export const Env = {
  make: (): Env => ({}),
  clone: (env: Env): Env => ({ ...env }),
  addPoly: (env: Env, name: string, ty: PolyTy): void => {
    env[name] = ty;
  },
  addMono: (env: Env, name: string, ty: MonoTy): void => {
    Env.addPoly(env, name, MonoTy.toPoly(ty));
  },
  lookup: (env: Env, name: string): Maybe<PolyTy> => {
    if (name in env) {
      return some(env[name]);
    }

    return none;
  },
  has: (env: Env, name: string): boolean => {
    return name in env;
  },
  freeTypeVars: (env: Env): Set<TyVarId> => {
    const freeTypeVars = new Set<TyVarId>();

    for (const ty of Object.values(env)) {
      addSet(freeTypeVars, PolyTy.freeTypeVars(ty));
    }

    return freeTypeVars;
  },
  show: (env: Env): string => {
    return '{ ' + joinWith(
      Object.entries(env),
      ([name, ty]) => `${name} : ${PolyTy.show(ty)}`,
      ', '
    ) + ' }';
  },
};