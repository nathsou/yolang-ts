import { Module } from "../ast/bitter";
import { zip } from "../utils/array";
import { Maybe, none, some } from "../utils/maybe";
import { proj } from "../utils/misc";
import { Env } from "./env";
import { MonoTy, TypeParam } from "./types";

export type TypeAlias = { ty: MonoTy, params: TypeParam[] };

export type TypeContext = {
  env: Env,
  typeParamsEnv: Record<string, MonoTy>,
  typeAliases: Record<string, TypeAlias>,
  modules: Map<string, Module>,
};

export const TypeContext = {
  make: (modules: Map<string, Module>): TypeContext => ({
    env: Env.make(),
    typeParamsEnv: {},
    typeAliases: {},
    modules,
  }),
  clone: (ctx: TypeContext): TypeContext => ({
    env: Env.clone(ctx.env),
    typeParamsEnv: { ...ctx.typeParamsEnv },
    typeAliases: { ...ctx.typeAliases },
    modules: ctx.modules,
  }),
  declareTypeAlias: (
    ctx: TypeContext,
    name: string,
    typeParams: TypeParam[],
    alias: MonoTy
  ): void => {
    TypeContext.declareTypeParams(ctx, ...typeParams);
    ctx.typeAliases[name] = {
      ty: alias,
      params: typeParams,
    };
  },
  declareTypeParams: (ctx: TypeContext, ...ps: TypeParam[]): void => {
    ps.forEach(p => {
      ctx.typeParamsEnv[p.name] = p.ty.match({
        Some: ty => ty,
        None: () => MonoTy.fresh(),
      });
    });
  },
  resolveTypeParam: (ctx: TypeContext, name: string): Maybe<MonoTy> => {
    if (name in ctx.typeParamsEnv) {
      return some(ctx.typeParamsEnv[name]);
    }

    return none;
  },
  instantiateTypeAlias: (ta: TypeAlias, params: MonoTy[]): MonoTy => {
    if (ta.params.length === 0) {
      return ta.ty;
    }

    const subst = new Map<string, MonoTy>(zip(ta.params.map(proj('name')), params));
    return MonoTy.substituteTyParams(ta.ty, subst);
  },
  resolveTypeAlias: (ctx: TypeContext, name: string): Maybe<{ ty: MonoTy, params: TypeParam[] }> => {
    if (name in ctx.typeAliases) {
      return some(ctx.typeAliases[name]);
    }

    return none;
  },
};
