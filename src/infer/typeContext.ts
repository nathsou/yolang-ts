import { Decl } from "../ast/bitter";
import { Maybe, none, some } from "../utils/maybe";
import { Env } from "./env";
import { MonoTy, TypeParam } from "./types";

export type TypeContext = {
  env: Env,
  typeParamsEnv: Record<string, MonoTy>,
  typeAliases: Record<string, { ty: MonoTy, params: TypeParam[] }>,
  topLevelDecls: Decl[],
  currentPath: string[],
};

export const TypeContext = {
  make: (topLevelDecls: Decl[]): TypeContext => ({
    env: Env.make(),
    typeParamsEnv: {},
    typeAliases: {},
    topLevelDecls,
    currentPath: [],
  }),
  clone: (ctx: TypeContext): TypeContext => ({
    env: Env.clone(ctx.env),
    typeParamsEnv: {},
    typeAliases: { ...ctx.typeAliases },
    topLevelDecls: [...ctx.topLevelDecls],
    currentPath: [...ctx.currentPath],
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
  findTypeAlias: (ctx: TypeContext, name: string): Maybe<[MonoTy, TypeParam[]]> => {
    if (name in ctx.typeAliases) {
      const { ty, params } = ctx.typeAliases[name];
      return some([ty, params]);
    }

    return none;
  },
};
