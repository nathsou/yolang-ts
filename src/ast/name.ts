import { MonoTy } from "../infer/types";
import { pushRecord } from "../utils/misc";

export type FuncName = {
  readonly original: string,
  renaming: string,
  ty: MonoTy,
};

export type VarName = FuncName & {
  readonly mutable: boolean,
  readonly isUndeclared: boolean,
};

export type TyParamName = {
  readonly name: string,
  ty: MonoTy,
};

export const FuncName = {
  fresh: (name: string): FuncName => ({
    original: name,
    renaming: name,
    ty: MonoTy.fresh(),
  }),
  clone: (name: FuncName, freshTy = true): FuncName => ({ ...name, ty: freshTy ? MonoTy.fresh() : name.ty }),
};

export const VarName = {
  fresh: (name: string, mutable: boolean, isUndeclared = false): VarName => ({
    original: name,
    renaming: name,
    ty: MonoTy.fresh(),
    mutable,
    isUndeclared,
  }),
  clone: (name: VarName, freshTy = true): VarName => ({ ...name, ty: freshTy ? MonoTy.fresh() : name.ty }),
};

export type NameEnv = {
  vars: Record<string, VarName>,
  funcs: Record<string, FuncName[]>,
};

export const NameEnv = {
  make: (): NameEnv => ({ vars: {}, funcs: {} }),
  clone: (env: NameEnv): NameEnv => ({ vars: { ...env.vars }, funcs: { ...env.funcs } }),
  declareVar: (env: NameEnv, name: string, mutable: boolean, renaming = name): VarName => {
    const fresh = VarName.fresh(name, mutable);
    fresh.renaming = renaming;
    env.vars[name] = fresh;
    return fresh;
  },
  declareFunc: (env: NameEnv, name: string, renaming = name): FuncName => {
    const fresh = FuncName.fresh(name);
    fresh.renaming = renaming;
    pushRecord(env.funcs, name, fresh);
    return fresh;
  },
  resolveVar: (env: NameEnv, name: string, renaming = name): VarName => {
    if (name in env.vars) {
      return env.vars[name];
    }

    const fresh = VarName.fresh(name, false, true);
    fresh.renaming = renaming;

    return fresh;
  },
  resolveFunc: (env: NameEnv, name: string, renaming = name): FuncName[] => {
    if (name in env.funcs) {
      return env.funcs[name];
    }

    const fresh = FuncName.fresh(name);
    fresh.renaming = renaming;

    return [fresh];
  },
  isUndeclared: (name: VarName): boolean => {
    return name.renaming === '<__!undeclared!__>';
  },
};