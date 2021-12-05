import { DataType, match as matchVariant } from "itsamatch";
import { Context } from "../ast/context";
import { Env } from "./env";

export type TyVarId = number;

// Monomorphic types
export type MonoTy = DataType<{
  TyVar: { id: TyVarId },
  TyConst: { name: string, args: MonoTy[] },
}>;

export const MonoTy = {
  TyVar: (id: TyVarId): MonoTy => ({ variant: 'TyVar', id }),
  TyConst: (name: string, args: MonoTy[]): MonoTy => ({ variant: 'TyConst', name, args }),
  toPoly: (ty: MonoTy): PolyTy => [[], ty],
  freeTypeVars: (ty: MonoTy, fvs: Set<TyVarId> = new Set()): Set<TyVarId> =>
    matchVariant(ty, {
      TyVar: ({ id }) => {
        fvs.add(id);
        return fvs;
      },
      TyConst: ({ args }) => {
        args.forEach(arg => {
          MonoTy.freeTypeVars(arg, fvs)
        });

        return fvs;
      },
    }),
  generalize: (env: Env, ty: MonoTy): PolyTy => {
    // can be optimized using (block instead of let) levels
    // https://github.com/tomprimozic/type-systems/tree/master/algorithm_w
    // https://okmij.org/ftp/ML/generalization.html
    const fvsTy = MonoTy.freeTypeVars(ty);
    const fvsEnv = Env.freeTypeVars(env);
    const quantified = [...diffSet(fvsTy, fvsEnv)];
    return PolyTy.make(quantified, ty);
  },
  fresh: () => {
    const id = Context.freshTyVarIndex();
    return MonoTy.TyVar(id);
  },
};

export type PolyTy = [TyVarId[], MonoTy];

export const PolyTy = {
  make: (quantifiedVars: TyVarId[], monoTy: MonoTy): PolyTy => [quantifiedVars, monoTy],
  freeTypeVars: ([quantified, monoTy]: PolyTy): Set<TyVarId> => {
    const freeVarsMonoTy = MonoTy.freeTypeVars(monoTy);
    return diffSet(freeVarsMonoTy, new Set(quantified));
  },
};