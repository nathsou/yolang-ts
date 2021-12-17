import { DataType, match as matchVariant } from "itsamatch";
import { match } from "ts-pattern";
import { Context } from "../ast/context";
import { gen, joinWith, zip } from "../utils/array";
import { cond, mapRecord, matchString, panic } from "../utils/misc";
import { diffSet } from "../utils/set";
import { Env } from "./env";
import { Row } from "./records";

export type TyVarId = number;
export type TyVar = { kind: 'Var', id: TyVarId } | { kind: 'Link', ref: MonoTy };

// Monomorphic types
export type MonoTy = DataType<{
  TyVar: { value: TyVar },
  TyConst: { name: string, args: MonoTy[] },
  TyFun: { args: MonoTy[], ret: MonoTy },
  TyRecord: { row: Row },
}>;

export const showTyVarId = (n: TyVarId): string => {
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
  TyRecord: (row: Row): MonoTy => ({ variant: 'TyRecord', row }),
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
        args.forEach(arg => {
          MonoTy.freeTypeVars(arg, fvs);
        });

        MonoTy.freeTypeVars(ret, fvs);

        return fvs;
      },
      TyRecord: ({ row }) => {
        Row.fields(row).forEach(([_, ty]) => {
          MonoTy.freeTypeVars(ty, fvs);
        });

        return fvs;
      },
      TyStruct: () => fvs,
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
      TyRecord: ({ row }) => {
        if (row.type === 'empty') {
          return ty;
        }

        return MonoTy.TyRecord(
          Row.extend(row.field,
            MonoTy.substitute(row.ty, subst),
            MonoTy.substitute(row.tail, subst)
          )
        );
      },
    });
  },
  show: (ty: MonoTy): string => matchVariant(ty, {
    TyVar: ({ value }) => {
      if (value.kind === 'Var') {
        return showTyVarId(value.id);
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
    TyRecord: ({ row }) => {
      if (row.type === 'empty') {
        return '{}';
      }

      return `{ ${joinWith(Row.fields(row).sort(([a], [b]) => a.localeCompare(b)), ([k, v]) => `${k}: ${MonoTy.show(v)}`, ', ')} }`;
    },
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

    const quantifiedVars = joinWith(quantified, showTyVarId, ', ');
    return `forall ${quantifiedVars}. ${MonoTy.show(monoTy)}`;
  },
  canonicalize: (poly: PolyTy): PolyTy => {
    const [vars, mono] = poly;
    const subts = new Map<TyVarId, MonoTy>();

    const newVars = gen(vars.length, i => i);

    for (const [v1, v2] of zip(vars, newVars)) {
      subts.set(v1, MonoTy.TyVar({ kind: 'Var', id: v2 }));
    }

    return [newVars, MonoTy.substitute(mono, subts)];
  },
};

export type TypeParamsContext = {
  typeParams: string[],
};

export const TypeParamsContext = {
  make: (): TypeParamsContext => ({ typeParams: [] }),
  declare: (ctx: TypeParamsContext, name: string): void => {
    ctx.typeParams.push(name);
  },
  has: (ctx: TypeParamsContext, name: string): boolean => {
    return ctx.typeParams.some(p => p === name);
  },
};

export type ParameterizedTy = DataType<{
  TyVar: { id: TyVarId },
  TyParam: { name: string },
  TyConst: { name: string, args: ParameterizedTy[] },
  TyFun: { args: ParameterizedTy[], ret: ParameterizedTy },
}>;

export const ParameterizedTy = {
  TyVar: (id: TyVarId): ParameterizedTy => ({ variant: 'TyVar', id }),
  TyParam: (name: string): ParameterizedTy => ({ variant: 'TyParam', name }),
  TyConst: (name: string, ...args: ParameterizedTy[]): ParameterizedTy => ({ variant: 'TyConst', name, args }),
  TyFun: (args: ParameterizedTy[], ret: ParameterizedTy): ParameterizedTy => ({ variant: 'TyFun', args, ret }),
  freeTyParams: (ty: ParameterizedTy): string[] => {
    return matchVariant(ty, {
      TyVar: () => [],
      TyParam: ({ name }) => [name],
      TyConst: ({ args }) => args.flatMap(ParameterizedTy.freeTyParams),
      TyFun: ({ args, ret }) => [...args.flatMap(ParameterizedTy.freeTyParams), ...ParameterizedTy.freeTyParams(ret)],
    });
  },
  substituteTyParams: (ty: ParameterizedTy, subst: Map<string, MonoTy>): MonoTy => {
    return matchVariant(ty, {
      TyVar: ({ id }) => MonoTy.TyVar({ kind: 'Var', id }),
      TyParam: ({ name }) => {
        if (!subst.has(name)) {
          panic(`Type parameter '${name}' not found in substitution`);
        }

        return subst.get(name)!;
      },
      TyConst: ({ name, args }) => MonoTy.TyConst(name, ...args.map(a => ParameterizedTy.substituteTyParams(a, subst))),
      TyFun: ({ args, ret }) => MonoTy.TyFun(
        args.map(a => ParameterizedTy.substituteTyParams(a, subst)),
        ParameterizedTy.substituteTyParams(ret, subst)
      ),
    });
  },
  toPoly: (ty: ParameterizedTy, params: string[]): PolyTy => {
    const tyParams = ParameterizedTy.freeTyParams(ty);
    const subst = new Map<string, MonoTy>();
    const tyVars: TyVarId[] = [];

    for (const t of params) {
      const v = Context.freshTyVarIndex();
      subst.set(t, MonoTy.TyVar({ kind: 'Var', id: v }));
      tyVars.push(v);
    }

    return PolyTy.make(tyVars, ParameterizedTy.substituteTyParams(ty, subst));
  },
  toMono: (ty: ParameterizedTy, params: Map<string, TyVarId>): MonoTy => {
    const subst = new Map<string, MonoTy>();

    for (const [name, id] of params.entries()) {
      subst.set(name, MonoTy.TyVar({ kind: 'Var', id: id }));
    }

    return ParameterizedTy.substituteTyParams(ty, subst);
  },
  isUnparameterized: (ty: ParameterizedTy): boolean => {
    return matchVariant(ty, {
      TyVar: () => true,
      TyParam: () => false,
      TyConst: ({ args }) => args.every(ParameterizedTy.isUnparameterized),
      TyFun: ({ args, ret }) => args.every(ParameterizedTy.isUnparameterized) && ParameterizedTy.isUnparameterized(ret),
    });
  },
  show: (ty: ParameterizedTy): string => {
    return matchVariant(ty, {
      TyParam: ({ name }) => `'${name}`,
      TyVar: ({ id }) => showTyVarId(id),
      _: () => MonoTy.show(ty as MonoTy),
    });
  },
};