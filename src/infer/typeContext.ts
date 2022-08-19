import { Module } from "../ast/bitter";
import { zip } from "../utils/array";
import { Maybe, none, some } from "../utils/maybe";
import { proj } from "../utils/misc";
import { Env } from "./env";
import { MonoTy, TypeParam } from "./types";
import { unifyPure } from "./unification";

export type TypeAliasInstance = { params: MonoTy[], ty: MonoTy };
export type TypeAlias = { ty: MonoTy, params: TypeParam[], instances: TypeAliasInstance[] };

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
      instances: [],
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
  instantiateTypeAlias: (ctx: TypeContext, ta: TypeAlias, params: MonoTy[]): MonoTy => {
    if (ta.params.length === 0) {
      return ta.ty;
    }

    for (const inst of ta.instances) {
      if (zip(params, inst.params).every(([p, q]) => unifyPure(p, q, ctx).isOk())) {
        return inst.ty;
      }
    }

    const subst = new Map<string, MonoTy>(zip(ta.params.map(proj('name')), params));
    const inst = MonoTy.substituteTyParams(ta.ty, subst);
    ta.instances.push({ ty: inst, params });

    return inst;
  },
  resolveTypeAlias: (ctx: TypeContext, name: string): Maybe<{ ty: MonoTy, params: TypeParam[], instances: TypeAliasInstance[] }> => {
    if (name in ctx.typeAliases) {
      return some(ctx.typeAliases[name]);
    }

    return none;
  },
};
