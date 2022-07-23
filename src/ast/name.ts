import { MonoTy } from "../infer/types";
import { panic } from "../utils/misc";

export type Name = {
  readonly original: string,
  renaming: string,
  ty: MonoTy,
  readonly mutable: boolean,
  isUndeclared: boolean,
};

export const Name = {
  fresh: (name: string, mutable: boolean, isUndeclared = false): Name => ({
    original: name,
    renaming: name,
    ty: MonoTy.fresh(),
    mutable,
    isUndeclared,
  }),
  clone: (name: Name, freshTy = true): Name => ({ ...name, ty: freshTy ? MonoTy.fresh() : name.ty }),
};

export type NameEnv = Record<string, Name>;

export const NameEnv = {
  make: (): NameEnv => ({}),
  clone: (env: NameEnv): NameEnv => ({ ...env }),
  declare: (env: NameEnv, name: string, mutable: boolean, renaming = name): Name => {
    if (name in env) {
      panic(`NameEnv.declare: name '${name}' already declared`);
    }

    const fresh = Name.fresh(name, mutable);
    fresh.renaming = renaming;
    env[name] = fresh;
    return fresh;
  },
  resolve: (env: NameEnv, name: string, renaming = name): Name => {
    if (name in env) {
      return env[name];
    }

    const fresh = Name.fresh(name, false, true);
    fresh.renaming = renaming;

    return fresh;
  },
  isUndeclared: (name: Name): boolean => {
    return name.renaming === '<__!undeclared!__>';
  },
};