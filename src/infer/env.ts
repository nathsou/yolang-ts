import { VariantOf } from "itsamatch";
import { Decl } from "../ast/bitter";
import { VarName } from "../ast/name";
import { joinWith } from "../utils/array";
import { Maybe, none, some } from "../utils/maybe";
import { pushRecord } from "../utils/misc";
import { addSet } from "../utils/set";
import { MonoTy, PolyTy, TyVarId } from "./types";

export type FuncDecl = VariantOf<Decl, 'Function'>;

export type Env = {
  vars: Record<string, { name: VarName, ty: PolyTy }>,
  funcs: Record<string, FuncDecl[]>,
};

export const Env = {
  make: (): Env => ({ vars: {}, funcs: {} }),
  clone: (env: Env): Env => ({ vars: { ...env.vars }, funcs: { ...env.funcs } }),
  addPolyVar: (env: Env, name: VarName, ty: PolyTy): void => {
    env.vars[name.original] = { name, ty };
  },
  declareFunc: (env: Env, f: FuncDecl): void => {
    pushRecord(env.funcs, f.name.original, f);
  },
  addMonoVar: (env: Env, name: VarName, ty: MonoTy): void => {
    Env.addPolyVar(env, name, MonoTy.toPoly(ty));
  },
  lookupVar: (env: Env, name: string): Maybe<{ name: VarName, ty: PolyTy }> => {
    if (name in env.vars) {
      return some(env.vars[name]);
    }

    return none;
  },
  lookupFuncs: (env: Env, name: string): FuncDecl[] => {
    if (name in env.funcs) {
      return env.funcs[name];
    }

    return [];
  },
  hasVar: (env: Env, name: string): boolean => {
    return name in env.vars;
  },
  freeTypeVars: (env: Env): Set<TyVarId> => {
    const freeTypeVars = new Set<TyVarId>();

    for (const { ty } of Object.values(env.vars)) {
      addSet(freeTypeVars, PolyTy.freeTypeVars(ty));
    }

    return freeTypeVars;
  },
  show: (env: Env): string => {
    return '{ ' + joinWith(
      Object.entries(env.vars),
      ([name, { ty }]) => `${name} : ${PolyTy.show(ty)}`,
      ', '
    ) + ' }';
  },
};