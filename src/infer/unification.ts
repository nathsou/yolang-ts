import { match as matchVariant } from "itsamatch";
import { match, __ } from "ts-pattern";
import { MonoTy, TyVarId } from "./types";

export type UnificationError = string;

// reduce type variable link chains:
// Link -> Link -> Link -> ty becomes Link -> ty
const simplifyLinks = (ty: MonoTy): MonoTy => {
  return matchVariant(ty, {
    TyVar: ({ value }) => {
      if (value.kind === 'Link') {
        const leaf = simplifyLinks(value.ref);
        value.ref = leaf;
        return leaf;
      } else {
        return ty;
      }
    },
    _: ty => ty,
  });
};

const occurs = (x: TyVarId, t: MonoTy): boolean => {
  return matchVariant(t, {
    TyVar: ({ value }) => {
      if (value.kind === 'Var') {
        return value.id === x;
      } else {
        simplifyLinks(t);
        return occurs(x, value.ref);
      }
    },
    TyConst: t => t.args.some(a => occurs(x, a)),
    TyFun: t => t.args.some(a => occurs(x, a)) || occurs(x, t.ret),
  });
};

const unifyMany = (eqs: [MonoTy, MonoTy][]): UnificationError[] => {
  const errors: UnificationError[] = [];

  while (eqs.length > 0) {
    const [s, t] = eqs.pop()!;

    match<[MonoTy, MonoTy]>([s, t])
      // Delete
      .when(([s, t]) => MonoTy.eq(s, t), () => { })
      // Eliminate
      .with([{ variant: 'TyVar', value: { kind: 'Var' } }, __], ([s, t]) => {
        if (occurs(s.value.id, t)) {
          errors.push(`occurs check failed: ${MonoTy.show(s)}, ${MonoTy.show(t)}`);
        } else {
          /// @ts-ignore
          s.value = { kind: 'Link', ref: t };
        }
      })
      .with([{ variant: 'TyVar', value: { kind: 'Link' } }, __], ([s, t]) => {
        eqs.push([s.value.ref, t]);
      })
      // Orient
      .with([__, { variant: 'TyVar' }], ([s, t]) => {
        eqs.push([t, s]);
      })
      // Decompose
      .with([{ variant: 'TyConst' }, { variant: 'TyConst' }], ([s, t]) => {
        if (s.name === t.name && s.args.length === t.args.length) {
          for (let i = 0; i < s.args.length; i++) {
            eqs.push([s.args[i], t.args[i]]);
          }
        }
      })
      .with([{ variant: 'TyFun' }, { variant: 'TyFun' }], ([s, t]) => {
        if (s.args.length === t.args.length) {
          for (let i = 0; i < s.args.length; i++) {
            eqs.push([s.args[i], t.args[i]]);
          }

          eqs.push([s.ret, t.ret]);
        }
      })
      .otherwise(([s, t]) => {
        errors.push(`unhandled case in unify: (${MonoTy.show(s)}, ${MonoTy.show(t)})`);
      });
  }

  return errors;
};

export const unify = (s: MonoTy, t: MonoTy): UnificationError[] => {
  return unifyMany([[s, t]]);
};