import { MonoTy } from "../infer/types";
import { panic, pushRecord } from "../utils/misc";

export type FuncName = {
  readonly original: string,
  mangled: string,
  ty: MonoTy,
};

export type VarName = FuncName & {
  readonly mutable: boolean,
  readonly initialized: boolean,
};

export type TyParamName = {
  readonly name: string,
  ty: MonoTy,
};

export const FuncName = {
  fresh: (name: string): FuncName => ({
    original: name,
    mangled: name,
    ty: MonoTy.fresh(),
  }),
  clone: (name: FuncName, freshTy = true): FuncName => ({ ...name, ty: freshTy ? MonoTy.fresh() : name.ty }),
  show: (name: FuncName) => name.mangled,
};

export const VarName = {
  fresh: (name: string, mutable: boolean, initialized = true): VarName => ({
    original: name,
    mangled: name,
    ty: MonoTy.fresh(),
    mutable,
    initialized,
  }),
  clone: (name: VarName, freshTy = true): VarName => ({ ...name, ty: freshTy ? MonoTy.fresh() : name.ty }),
  show: (name: VarName) => name.mangled,
};

export type NameEnv = {
  vars: Record<string, VarName>,
  funcs: Record<string, FuncName[]>,
  params: Record<string, MonoTy>,
};

export const NameEnv = {
  make: (): NameEnv => ({ vars: {}, funcs: {}, params: {} }),
  clone: (env: NameEnv): NameEnv => ({ vars: { ...env.vars }, funcs: { ...env.funcs }, params: { ...env.params } }),
  declareVar: (env: NameEnv, name: string, mutable: boolean, renaming = name): VarName => {
    const fresh = VarName.fresh(name, mutable);
    fresh.mangled = renaming;
    env.vars[name] = fresh;
    return fresh;
  },
  declareFunc: (env: NameEnv, name: string, renaming = name): FuncName => {
    const fresh = FuncName.fresh(name);
    fresh.mangled = renaming;
    pushRecord(env.funcs, name, fresh);
    return fresh;
  },
  declareTypeParam: (env: NameEnv, name: string, ty: MonoTy = MonoTy.Param(name)): MonoTy => {

    env.params[name] = ty;
    return ty;
  },
  resolveVar: (env: NameEnv, name: string): VarName => {
    if (name in env.vars) {
      return env.vars[name];
    }

    return panic(`unresolved variable '${name}'`);
  },
  resolveFunc: (env: NameEnv, name: string): FuncName[] => {
    if (name in env.funcs) {
      return env.funcs[name];
    }

    return panic(`unresolved function '${name}'`);
  },
  resolveTypeParam: (env: NameEnv, name: string): MonoTy => {
    if (name in env.params) {
      return env.params[name];
    }

    return panic(`unresolved type parameter '${name}'`);
  },
  resolveType: (env: NameEnv, ty: MonoTy): MonoTy => {
    return MonoTy.rewrite(ty, t => {
      if (t.variant === 'Const' && t.args.length === 0 && t.name in env.params) {
        return env.params[t.name];
      }

      return t;
    });
  },
  isUndeclared: (name: VarName): boolean => {
    return name.mangled === '<__!undeclared!__>';
  },
};