import { VariantOf } from "itsamatch";
import { Decl } from "../ast/bitter";
import { Context } from "../ast/context";
import { Error } from "../errors/errors";
import { Maybe, none, some } from "../utils/maybe";
import { pushRecord } from "../utils/misc";
import { Env } from "./env";
import { Impl, TraitImpl } from "./impls";
import { Subst } from "./subst";
import { Trait } from "./traits";
import { MonoTy, PolyTy, TypeParams, TyVar } from "./types";
import { unifyPure } from "./unification";

export type TypeContext = {
  env: Env,
  typeParamsEnv: Record<string, TyVar>,
  modules: Record<string, VariantOf<Decl, 'Module'>>,
  typeAliases: Record<string, { ty: MonoTy, params: TypeParams }>,
  impls: Record<string, Impl[]>,
  traits: Record<string, Trait>,
  traitImpls: Record<string, TraitImpl[]>,
  topLevelDecls: Decl[],
};

export const TypeContext = {
  make: (topLevelDecls: Decl[]): TypeContext => ({
    env: Env.make(),
    typeParamsEnv: {},
    modules: {},
    typeAliases: {},
    impls: {},
    traits: {},
    traitImpls: {},
    topLevelDecls,
  }),
  clone: (ctx: TypeContext): TypeContext => ({
    env: Env.clone(ctx.env),
    typeParamsEnv: {},
    modules: { ...ctx.modules },
    typeAliases: { ...ctx.typeAliases },
    impls: { ...ctx.impls },
    traits: { ...ctx.traits },
    traitImpls: { ...ctx.traitImpls },
    topLevelDecls: [...ctx.topLevelDecls],
  }),
  declareModule: (ctx: TypeContext, mod: VariantOf<Decl, 'Module'>): void => {
    ctx.modules[mod.name] = mod;
  },
  declareTypeAlias: (
    ctx: TypeContext,
    name: string,
    typeParams: TypeParams,
    alias: MonoTy
  ): void => {
    TypeContext.declareTypeParams(ctx, ...typeParams);
    ctx.typeAliases[name] = {
      ty: alias,
      params: typeParams,
    };
  },
  declareImpl: (ctx: TypeContext, impl: Impl): void => {
    for (const name of Object.keys(impl.methods)) {
      pushRecord(ctx.impls, name, impl);
    }
  },
  declareTraitImpl: (ctx: TypeContext, impl: VariantOf<Decl, 'TraitImpl'>): Maybe<TraitImpl> => {
    const trait = TypeContext.findTrait(ctx, impl.trait.path, impl.trait.name);

    return trait.map(trait => {
      const imp = TraitImpl.from(impl, trait);
      pushRecord(ctx.traitImpls, TraitImpl.hash(impl), imp);
      return imp;
    });
  },
  declareTrait: (ctx: TypeContext, trait: Trait): void => {
    ctx.traits[trait.name] = trait;
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
  findTrait(ctx: TypeContext, path: string[], name: string): Maybe<Trait> {
    const mod = TypeContext.resolveModule(ctx, path);

    return mod.flatMap(mod => {
      const trait = mod.decls.find(decl =>
        decl.variant === 'Trait' &&
        decl.name === name
      );

      if (trait) {
        return some(trait);
      }

      return none;
    });
  },
  findImplMethod: (ctx: TypeContext, funcName: string, ty: MonoTy): Maybe<[Impl, Subst, MonoTy, TypeContext]> => {
    if (funcName in ctx.impls) {
      for (const impl of ctx.impls[funcName]) {
        const newCtx = TypeContext.clone(ctx);
        const instTy = PolyTy.instantiate(PolyTy.instantiateTyParams(impl.typeParams, impl.ty));
        TypeContext.declareTypeParams(newCtx, ...impl.typeParams);
        const res = unifyPure(ty, instTy, newCtx);
        if (res.isOk()) {
          return some([impl, res.unwrap(), instTy, newCtx]);
        }
      }
    }

    return none;
  },
  findTypeAlias: (ctx: TypeContext, path: string[], name: string): Maybe<[MonoTy, TypeParams]> => {
    const aux = (path: string[], decls: Decl[]): Maybe<[MonoTy, TypeParams]> => {
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
};