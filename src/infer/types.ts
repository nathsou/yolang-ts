import { DataType, match as matchVariant } from "itsamatch";
import { match } from "ts-pattern";
import { Context } from "../ast/context";
import { joinWith } from "../utils/array";
import { cond, matchString } from "../utils/misc";
import { diffSet } from "../utils/set";
import { Env } from "./env";

export type TyVarId = number;
export type TyVar = { kind: 'Var', id: TyVarId } | { kind: 'Link', ref: MonoTy };

// Monomorphic types
export type MonoTy = DataType<{
  TyVar: { value: TyVar },
  TyConst: { name: string, args: MonoTy[] },
  TyFun: { args: MonoTy[], ret: MonoTy },
}>;

const showTyIndex = (n: TyVarId): string => {
  const l = String.fromCharCode(97 + n % 26);
  return n >= 23 ? `${l}${Math.floor(n / 26)}` : l;
};

export const MonoTy = {
  TyVar: (tv: TyVar): MonoTy => {
    if (tv.kind === 'Link') {
      return { variant: 'TyVar', value: { kind: 'Link', ref: MonoTy.deref(tv.ref) } };
    }

    return { variant: 'TyVar', value: tv };
  },
  TyConst: (name: string, ...args: MonoTy[]): MonoTy => ({ variant: 'TyConst', name, args }),
  TyFun: (args: MonoTy[], ret: MonoTy): MonoTy => ({ variant: 'TyFun', args, ret }),
  toPoly: (ty: MonoTy): PolyTy => [[], ty],
  u32: () => MonoTy.TyConst('u32'),
  bool: () => MonoTy.TyConst('bool'),
  unit: () => MonoTy.TyConst('()'),
  tuple: (tys: MonoTy[]) => MonoTy.TyConst('tuple', ...tys),
  freeTypeVars: (ty: MonoTy, fvs: Set<TyVarId> = new Set()): Set<TyVarId> =>
    matchVariant(ty, {
      TyVar: ({ value }) => {
        if (value.kind === 'Var') {
          fvs.add(value.id);
        } else {
          MonoTy.freeTypeVars(value.ref, fvs);
        }
        return fvs;
      },
      TyConst: ({ args }) => {
        args.forEach(arg => {
          MonoTy.freeTypeVars(arg, fvs)
        });

        return fvs;
      },
      TyFun: ({ args, ret }) => {
        const fvs = new Set<TyVarId>();

        args.forEach(arg => {
          MonoTy.freeTypeVars(arg, fvs);
        });

        MonoTy.freeTypeVars(ret, fvs);

        return fvs;
      }
    }),
  generalize: (env: Env, ty: MonoTy): PolyTy => {
    // can be optimized using (block instead of let) levels
    // https://github.com/tomprimozic/type-systems/tree/master/algorithm_w
    // https://okmij.org/ftp/ML/generalization.html
    const fvsTy = MonoTy.freeTypeVars(ty);
    const fvsEnv = Env.freeTypeVars(env);
    const quantified = [...diffSet(fvsTy, fvsEnv)];

    return PolyTy.make(quantified, ty);
  },
  fresh: () => {
    const id = Context.freshTyVarIndex();
    return MonoTy.TyVar({ kind: 'Var', id });
  },
  deref: (ty: MonoTy): MonoTy => {
    if (ty.variant === 'TyVar' && ty.value.kind === 'Link') {
      const ret = MonoTy.deref(ty.value.ref);
      ty.value.ref = ret;
      return ret;
    }

    return ty;
  },
  substitute: (ty: MonoTy, subst: Map<TyVarId, MonoTy>): MonoTy => {
    return matchVariant(ty, {
      TyVar: ({ value }) => {
        if (value.kind === 'Var') {
          if (subst.has(value.id)) {
            return subst.get(value.id)!;
          } else {
            return ty;
          }
        } else {
          return MonoTy.substitute(value.ref, subst);
        }
      },
      TyConst: ({ name, args }) => MonoTy.TyConst(
        name,
        ...args.map(arg => MonoTy.substitute(arg, subst))
      ),
      TyFun: ({ args, ret }) => MonoTy.TyFun(
        args.map(arg => MonoTy.substitute(arg, subst)),
        MonoTy.substitute(ret, subst)
      ),
    });
  },
  show: (ty: MonoTy): string => matchVariant(ty, {
    TyVar: ({ value }) => {
      if (value.kind === 'Var') {
        return showTyIndex(value.id);
      } else {
        return MonoTy.show(value.ref);
      }
    },
    TyConst: ({ name, args }) => cond(args.length === 0, {
      then: () => name,
      else: () => matchString(name, {
        'tuple': () => `(${joinWith(args, MonoTy.show, ', ')})`,
        _: () => `(${name} ${joinWith(args, MonoTy.show, ', ')})`,
      }),
    }),
    TyFun: ({ args, ret }) => cond(args.length === 1, {
      then: () =>
        match(MonoTy.deref(args[0]))
          .with({ variant: 'TyConst', name: 'tuple' }, () => `(${MonoTy.show(args[0])}) -> ${MonoTy.show(ret)}`)
          .otherwise(a => `${MonoTy.show(a)} -> ${MonoTy.show(ret)}`),
      else: () => `(${joinWith(args, MonoTy.show, ', ')}) -> ${MonoTy.show(ret)}`,
    }),
  }),
  eq: (s: MonoTy, t: MonoTy): boolean =>
    match<[MonoTy, MonoTy]>([s, t])
      .with(
        [
          { variant: 'TyVar', value: { kind: 'Var' } },
          { variant: 'TyVar', value: { kind: 'Var' } },
        ],
        ([{ value: s }, { value: t }]) => s.id === t.id
      )
      .with(
        [{ variant: 'TyVar' }, { variant: 'TyVar' }],
        () => MonoTy.eq(MonoTy.deref(s), MonoTy.deref(t)),
      )
      .with(
        [{ variant: 'TyConst' }, { variant: 'TyConst' }],
        ([s, t]) => {
          if (s.name === t.name && s.args.length === t.args.length) {
            return s.args.every((arg, i) => MonoTy.eq(arg, t.args[i]));
          }

          return false;
        }
      )
      .with(
        [{ variant: 'TyFun' }, { variant: 'TyFun' }],
        ([s, t]) => {
          if (s.args.length === t.args.length && MonoTy.eq(s.ret, t.ret)) {
            return s.args.every((arg, i) => MonoTy.eq(arg, t.args[i]));
          }

          return false;
        }
      )
      .otherwise(() => false),
};

export type PolyTy = [TyVarId[], MonoTy];

export const PolyTy = {
  make: (quantifiedVars: TyVarId[], monoTy: MonoTy): PolyTy => [quantifiedVars, monoTy],
  fresh: () => MonoTy.toPoly(MonoTy.fresh()),
  instantiate: ([quantifiedVars, ty]: PolyTy): MonoTy => {
    // replace all bound type variables with fresh type variables
    const subst = new Map<TyVarId, MonoTy>();
    quantifiedVars.forEach(id => {
      subst.set(id, MonoTy.fresh());
    });

    return MonoTy.substitute(ty, subst);
  },
  freeTypeVars: ([quantified, monoTy]: PolyTy): Set<TyVarId> => {
    const freeVarsMonoTy = MonoTy.freeTypeVars(monoTy);
    return diffSet(freeVarsMonoTy, new Set(quantified));
  },
  show: ([quantified, monoTy]: PolyTy): string => {
    if (quantified.length === 0) {
      return MonoTy.show(monoTy);
    }

    const quantifiedVars = joinWith(quantified, showTyIndex, ', ');
    return `forall ${quantifiedVars}. ${MonoTy.show(monoTy)}`;
  },
};