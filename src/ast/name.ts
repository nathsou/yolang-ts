import { MonoTy } from "../infer/types";

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
};

export type NameEnv = Record<string, Name>;

export const NameEnv = {
  make: (): NameEnv => ({}),
  clone: (env: NameEnv): NameEnv => ({ ...env }),
  declare: (env: NameEnv, name: string, mutable: boolean): Name => {
    const fresh = Name.fresh(name, mutable);
    env[name] = fresh;
    return fresh;
  },
  resolve: (env: NameEnv, name: string): Name => {
    if (name in env) {
      return env[name];
    }

    return Name.fresh(name, false, true);
  },
  isUndeclared: (name: Name): boolean => {
    return name.renaming === '<__!undeclared!__>';
  },
};