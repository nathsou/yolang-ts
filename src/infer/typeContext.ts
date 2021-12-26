import { match as matchVariant, VariantOf } from "itsamatch";
import { Decl } from "../ast/bitter";
import { Context } from "../ast/context";
import { Maybe, none, some } from "../utils/maybe";
import { pushRecord } from "../utils/misc";
import { Env } from "./env";
import { Impl } from "./impls";
import { RowGeneric } from "./records";
import { Subst } from "./subst";
import { MonoTy, ParameterizedTy, TypeParams, TyVar } from "./types";
import { unifyPure } from "./unification";

export type TypeContext = {
  env: Env,
  typeParamsEnv: Record<string, TyVar>,
  modules: Record<string, VariantOf<Decl, 'Module'>>,
  typeAliases: Record<string, { ty: ParameterizedTy, params: TypeParams }>,
  impls: Record<string, Impl[]>,
};

export const TypeContext = {
  make: (): TypeContext => ({
    env: Env.make(),
    typeParamsEnv: {},
    modules: {},
    typeAliases: {},
    impls: {},
  }),
  clone: (ctx: TypeContext): TypeContext => ({
    env: Env.clone(ctx.env),
    modules: { ...ctx.modules },
    typeAliases: { ...ctx.typeAliases },
    impls: { ...ctx.impls },
    typeParamsEnv: {},
  }),
  declareModule: (ctx: TypeContext, mod: VariantOf<Decl, 'Module'>): void => {
    ctx.modules[mod.name] = mod;
  },
  declareTypeAlias: (
    ctx: TypeContext,
    name: string,
    typeParams: TypeParams,
    alias: ParameterizedTy
  ): void => {
    TypeContext.declareTypeParams(ctx, ...typeParams);
    ctx.typeAliases[name] = {
      ty: alias,
      params: typeParams,
    };
  },
  declareImpl: (ctx: TypeContext, impl: VariantOf<Decl, 'Impl'>): void => {
    const imp = Impl.from(impl);

    for (const decl of impl.decls) {
      if (decl.variant === 'Function') {
        pushRecord(ctx.impls, decl.name.original, imp);
      }
    }
  },
  declareTypeParams: (ctx: TypeContext, ...names: string[]): void => {
    for (const name of names) {
      ctx.typeParamsEnv[name] = TyVar.Unbound(Context.freshTyVarIndex());
    }
  },
  // TODO: cleanup
  resolveModule: (ctx: TypeContext, path: string[]): Maybe<VariantOf<Decl, 'Module'>> => {
    let members: Record<string, Decl> = ctx.modules;
    let mod: VariantOf<Decl, 'Module'> | undefined;

    for (const name of path) {
      const decl = members[name];
      if (decl !== undefined && decl.variant === 'Module') {
        mod = decl;
        members = mod.members;
      } else {
        return none;
      }
    }

    return mod ? some(mod) : none;
  },
  findImplMethod: (ctx: TypeContext, funcName: string, ty: MonoTy): Maybe<[Impl, Subst, MonoTy, TypeContext]> => {
    if (funcName in ctx.impls) {
      for (const impl of ctx.impls[funcName]) {
        const newCtx = TypeContext.clone(ctx);
        TypeContext.declareTypeParams(newCtx, ...impl.typeParams);
        const implTyInst = ParameterizedTy.instantiate(impl.ty, newCtx);
        const res = unifyPure(ty, implTyInst, newCtx);
        if (res.isOk()) {
          return some([impl, res.unwrap(), implTyInst, newCtx]);
        }
      }
    }

    return none;
  },
};