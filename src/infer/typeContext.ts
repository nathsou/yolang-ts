import { Module } from "../ast/bitter";
import { zip, last } from "../utils/array";
import { Maybe, none, some } from "../utils/maybe";
import { proj } from "../utils/misc";
import { Env } from "./env";
import { MonoTy, TypeParam } from "./types";

export type TypeAlias = { ty: MonoTy, params: TypeParam[] };
export type FuncInfo = { returnTy: MonoTy };

export type TypeContext = {
  env: Env,
  typeParamsEnv: Map<string, MonoTy>,
  typeAliases: Map<string, TypeAlias>,
  modules: Map<string, Module>,
  funcStack: FuncInfo[],
};

export const TypeContext = {
  make: (modules: Map<string, Module>): TypeContext => ({
    env: Env.make(),
    typeParamsEnv: new Map(),
    typeAliases: new Map(),
    modules,
    funcStack: [],
  }),
  clone: (ctx: TypeContext): TypeContext => ({
    env: Env.clone(ctx.env),
    typeParamsEnv: new Map(ctx.typeParamsEnv),
    typeAliases: new Map(ctx.typeAliases),
    modules: new Map(ctx.modules),
    funcStack: [...ctx.funcStack],
  }),
  declareTypeAlias: (
    ctx: TypeContext,
    name: string,
    typeParams: TypeParam[],
    alias: MonoTy
  ): void => {
    // TypeContext.declareTypeParams(ctx, ...typeParams);
    const fullTy = MonoTy.expand(alias, ctx);
    if (fullTy.variant === 'Struct') {
      fullTy.name = name;
    }

    ctx.typeAliases.set(name, {
      ty: fullTy,
      params: typeParams,
    });
  },
  declareTypeParams: (ctx: TypeContext, ...ps: TypeParam[]): void => {
    ps.forEach(p => {
      ctx.typeParamsEnv.set(p.name, p.ty);
    });
  },
  resolveTypeParam: (ctx: TypeContext, name: string): Maybe<MonoTy> => {
    if (ctx.typeParamsEnv.has(name)) {
      return some(ctx.typeParamsEnv.get(name)!);
    }

    return none;
  },
  instantiateTypeAlias: (ctx: TypeContext, ta: TypeAlias, params: MonoTy[]): MonoTy => {
    if (ta.params.length === 0) {
      return ta.ty;
    }

    const subst = new Map<string, MonoTy>(zip(ta.params.map(proj('name')), params.map(ty => MonoTy.expand(ty, ctx))));
    return MonoTy.substituteTyParams(ta.ty, subst);
  },
  resolveTypeAlias: (ctx: TypeContext, name: string): Maybe<TypeAlias> => {
    if (ctx.typeAliases.has(name)) {
      return some(ctx.typeAliases.get(name)!);
    }

    return none;
  },
  getCurrentFunc: (ctx: TypeContext): Maybe<FuncInfo> => {
    if (ctx.funcStack.length === 0) {
      return none;
    }

    return some(last(ctx.funcStack));
  },
};
