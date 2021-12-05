import { MonoTy, TyVarId } from "../infer/types";

export type Context = {
  typeVarIndex: TyVarId,
};

export const context: Context = {
  typeVarIndex: 0,
};

export const Context = {
  freshTyVarIndex: (): TyVarId => {
    context.typeVarIndex += 1;
    return context.typeVarIndex;
  },
};