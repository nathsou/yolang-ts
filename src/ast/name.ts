import { MonoTy } from "../infer/types";

export type Name = {
  readonly original: string,
  renaming: string,
  ty: MonoTy,
  readonly mutable: boolean,
};

export const Name = {
  fresh: (name: string, mutable: boolean): Name => ({
    original: name,
    renaming: name,
    ty: MonoTy.fresh(),
    mutable,
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

    // TODO: return a dummy value
    // so that this error gets handled in the inferencer
    throw `Name '${name}' not found`;
  },
};