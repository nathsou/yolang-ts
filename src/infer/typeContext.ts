import { VariantOf } from "itsamatch";
import { Decl } from "../ast/bitter";
import { Maybe, none, some } from "../utils/maybe";
import { pushRecord } from "../utils/misc";
import { Env } from "./env";
import { Impl } from "./impls";
import { Subst } from "./subst";
import { MonoTy } from "./types";
import { unifyPure } from "./unification";

export type TypeContext = {
  env: Env,
  modules: Record<string, VariantOf<Decl, 'Module'>>,
  typeDecls: Record<string, VariantOf<Decl, 'NamedRecord'>>,
  impls: Record<string, Impl[]>,
};

export const TypeContext = {
  make: (): TypeContext => ({ env: Env.make(), modules: {}, typeDecls: {}, impls: {} }),
  clone: (ctx: TypeContext): TypeContext => ({
    env: Env.clone(ctx.env),
    modules: { ...ctx.modules },
    typeDecls: { ...ctx.typeDecls },
    impls: { ...ctx.impls },
  }),
  declareModule: (ctx: TypeContext, mod: VariantOf<Decl, 'Module'>): void => {
    ctx.modules[mod.name] = mod;
  },
  declareType: (ctx: TypeContext, ty: VariantOf<Decl, 'NamedRecord'>): void => {
    ctx.typeDecls[ty.name] = ty;
  },
  declareImpl: (ctx: TypeContext, impl: VariantOf<Decl, 'Impl'>): void => {
    const imp = Impl.from(impl);

    for (const decl of impl.decls) {
      if (decl.variant === 'Function') {
        pushRecord(ctx.impls, decl.name.original, imp);
      }
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
  findImplMethod: (ctx: TypeContext, funcName: string, ty: MonoTy): Maybe<[Impl, Subst]> => {
    if (funcName in ctx.impls) {
      for (const impl of ctx.impls[funcName]) {
        const res = unifyPure(impl.ty, ty);
        if (res.isOk()) {
          return some([impl, res.unwrap()]);
        }
      }
    }

    return none;
  },
};