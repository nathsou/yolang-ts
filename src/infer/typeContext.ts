import { match, VariantOf } from "itsamatch";
import { Decl } from "../ast/bitter";
import { Imports } from "../ast/sweet";
import { Maybe, none, some } from "../utils/maybe";
import { Env } from "./env";
import { MonoTy, TypeParam } from "./types";

export type TypeContext = {
  env: Env,
  typeParamsEnv: Record<string, MonoTy>,
  modules: Record<string, VariantOf<Decl, 'Module'>>,
  typeAliases: Record<string, { ty: MonoTy, params: TypeParam[] }>,
  topLevelDecls: Decl[],
  currentPath: string[],
};

export const TypeContext = {
  make: (topLevelDecls: Decl[]): TypeContext => ({
    env: Env.make(),
    typeParamsEnv: {},
    modules: {},
    typeAliases: {},
    topLevelDecls,
    currentPath: [],
  }),
  clone: (ctx: TypeContext): TypeContext => ({
    env: Env.clone(ctx.env),
    typeParamsEnv: {},
    modules: { ...ctx.modules },
    typeAliases: { ...ctx.typeAliases },
    topLevelDecls: [...ctx.topLevelDecls],
    currentPath: [...ctx.currentPath],
  }),
  declareModule: (ctx: TypeContext, mod: VariantOf<Decl, 'Module'>): void => {
    ctx.modules[mod.name] = mod;
  },
  enterModule: (ctx: TypeContext, name: string): void => {
    ctx.currentPath.push(name);
  },
  exitModule: (ctx: TypeContext): void => {
    ctx.currentPath.pop();
  },
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
  // TODO: cleanup
  __resolveModuleAux: (ctx: TypeContext, path: string[]): Maybe<VariantOf<Decl, 'Module'>> => {
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
  resolveModule: (ctx: TypeContext, path: string[]): Maybe<VariantOf<Decl, 'Module'>> => {
    // try to resolve in local scope or in global module
    return TypeContext.__resolveModuleAux(ctx, [...ctx.currentPath, ...path]).or(
      TypeContext.__resolveModuleAux(ctx, path)
    );
  },
  findTypeAlias: (ctx: TypeContext, path: string[], name: string): Maybe<[MonoTy, TypeParam[]]> => {
    const aux = (path: string[], decls: Decl[]): Maybe<[MonoTy, TypeParam[]]> => {
      if (path.length === 0) {
        const typeAlias = decls.find(d => d.variant === 'TypeAlias' && d.name === name);
        if (typeAlias) {
          const { alias, typeParams } = (typeAlias as VariantOf<Decl, 'TypeAlias'>);
          return some([alias, typeParams]);
        }
      }

      const [head, ...tail] = path;

      const mod = decls.find(decl => decl.variant === 'Module' && decl.name === head);

      if (mod) {
        return aux(tail, (mod as VariantOf<Decl, 'Module'>).decls);
      }

      return none;
    };

    if (name in ctx.typeAliases) {
      const { ty, params } = ctx.typeAliases[name];
      return some([ty, params]);
    }

    const moduleName = path.length > 0 ? path[0] : name;
    const mod = ctx.modules[moduleName];

    if (mod) {
      return aux(path.slice(1), mod.decls);
    }

    return none;
  },
  bringIntoScope: (ctx: TypeContext, path: string[], imports: Imports): Maybe<VariantOf<Decl, 'Module'>> => {
    const mod = TypeContext.resolveModule(ctx, path);
    const isImported = (name: string) => imports.variant === 'all' || imports.names.has(name);

    mod.do(mod => {
      for (const decl of mod.decls) {
        match(decl, {
          Module: m => {
            if (isImported(m.name)) {
              TypeContext.declareModule(ctx, m);
            }
          },
          TypeAlias: ({ alias, typeParams, name }) => {
            if (isImported(name)) {
              TypeContext.declareTypeAlias(ctx, name, typeParams, alias);
            }
          },
          Use: () => { },
          Function: f => {
            if (isImported(f.name.original)) {
              Env.declareFunc(ctx.env, f);
            }
          },
          Error: () => { },
        });
      }
    });

    return mod;
  },
};