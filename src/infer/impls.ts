import { VariantOf } from "itsamatch";
import { Decl } from "../ast/bitter";
import { Trait } from "./traits";
import { MonoTy, TypeParams } from "./types";

export interface Impl {
  ty: MonoTy,
  typeParams: TypeParams,
  methods: Record<string, VariantOf<Decl, 'Function'>>,
  staticFuncs: Record<string, VariantOf<Decl, 'Function'>>,
}

export const Impl = {
  from: (ty: MonoTy, typeParams: TypeParams, decls: Decl[]): Impl => {
    const impl: Impl = {
      ty,
      typeParams,
      methods: {},
      staticFuncs: {},
    };

    for (const decl of decls) {
      if (decl.variant === 'Function') {
        const { name, args } = decl;
        const isMethod =
          args.length > 0 &&
          args[0].name.original === 'self';

        if (isMethod) {
          impl.methods[name.original] = decl;
        } else {
          impl.staticFuncs[name.original] = decl;
        }
      }
    }

    return impl;
  },
};

export interface TraitImpl extends Impl {
  trait: Trait,
  implementee: MonoTy,
  args: MonoTy[],
}

export const TraitImpl = {
  from: (
    { implementee, methods, typeParams, trait: { args } }: VariantOf<Decl, 'TraitImpl'>,
    trait: Trait,
  ): TraitImpl => {
    return {
      implementee,
      trait,
      args,
      ...Impl.from(implementee, typeParams, Object.values(methods)),
    };
  },
  hash: ({ trait: { path, name } }: VariantOf<Decl, 'TraitImpl'>): string => {
    return [...path, name].join('.');
  },
};