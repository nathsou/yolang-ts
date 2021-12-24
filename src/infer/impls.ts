import { VariantOf } from "itsamatch";
import { Decl } from "../ast/bitter";
import { MonoTy } from "./types";

export type Impl = {
  ty: MonoTy,
  methods: Record<string, VariantOf<Decl, 'Function'>>,
  staticFuncs: Record<string, VariantOf<Decl, 'Function'>>,
};

export const Impl = {
  from: ({ ty, decls }: VariantOf<Decl, 'Impl'>): Impl => {
    const impl: Impl = {
      ty,
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