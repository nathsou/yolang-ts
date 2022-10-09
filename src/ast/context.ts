import { TyVarId } from "../infer/types";
import { assert } from "../utils/misc";

export type Context = {
  typeVarIndex: TyVarId,
  arch: 0 | 32 | 64,
};

const context: Context = {
  typeVarIndex: 0,
  arch: 0,
};

export const Context = {
  clear: (): void => {
    context.typeVarIndex = 0;
    context.arch = 0;
  },
  freshTyVarIndex: (): TyVarId => {
    context.typeVarIndex += 1;
    return context.typeVarIndex;
  },
  arch: (): 32 | 64 => {
    assert(context.arch !== 0, 'unknown processor architecture');
    return context.arch;
  },
  setArch: (arch: 32 | 64): void => {
    context.arch = arch;
  },
};
