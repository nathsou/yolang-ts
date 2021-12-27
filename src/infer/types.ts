import { DataType, match as matchVariant } from "itsamatch";
import { match } from "ts-pattern";
import { Context } from "../ast/context";
import { gen, joinWith, zip } from "../utils/array";
import { cond, mapRecord, matchString, panic, parenthesized } from "../utils/misc";
import { diffSet } from "../utils/set";
import { Env } from "./env";
import { Row, RowGeneric, RowMono } from "./records";
import { TupleGeneric, TupleMono } from "./tuples";
import { TypeContext } from "./typeContext";

export type TyVarId = number;
export type TyVar = DataType<{
  Unbound: { id: TyVarId },
  Link: { to: MonoTy },
}, 'kind'>;

export const TyVar = {
  Unbound: (id: TyVarId): TyVar => ({ kind: 'Unbound', id }),
  Link: (to: MonoTy): TyVar => ({ kind: 'Link', to }),
  fresh: (): TyVar => TyVar.Unbound(Context.freshTyVarIndex()),
};

// Monomorphic types
export type MonoTy = DataType<{
  Var: { value: TyVar },
  Const: { name: string, args: MonoTy[] },
  Fun: { args: MonoTy[], ret: MonoTy },
  Tuple: { tuple: TupleMono },
  Record: { row: RowMono },
  NamedRecord: { name: string, row: RowMono },
}>;

export const showTyVarId = (n: TyVarId): string => {
  const l = String.fromCharCode(65 + n % 26);
  return n >= 26 ? `${l}${Math.floor(n / 26)}` : l;
};

export const MonoTy = {
  Var: (tv: TyVar) => matchVariant(tv, {
    Unbound: (tv): MonoTy => ({ variant: 'Var', value: tv }),
    Link: ({ to }): MonoTy => ({ variant: 'Var', value: { kind: 'Link', to: MonoTy.deref(to) } }),
  }, 'kind'),
  Const: (name: string, ...args: MonoTy[]): MonoTy => ({ variant: 'Const', name, args }),
  Fun: (args: MonoTy[], ret: MonoTy): MonoTy => ({ variant: 'Fun', args, ret }),
  Tuple: (tuple: TupleMono): MonoTy => ({ variant: 'Tuple', tuple }),
  Record: (row: RowMono): MonoTy => ({ variant: 'Record', row }),
  NamedRecord: (name: string, fields: Readonly<RowMono>): MonoTy => ({ variant: 'NamedRecord', name, row: fields }),
  toPoly: (ty: MonoTy): PolyTy => [[], ty],
  u32: () => MonoTy.Const('u32'),
  bool: () => MonoTy.Const('bool'),
  unit: () => MonoTy.Const('()'),
  freeTypeVars: (ty: MonoTy, fvs: Set<TyVarId> = new Set()): Set<TyVarId> =>
    matchVariant(ty, {
      Var: ({ value }) => {
        if (value.kind === 'Unbound') {
          fvs.add(value.id);
        } else {
          MonoTy.freeTypeVars(value.to, fvs);
        }

        return fvs;
      },
      Const: ({ args }) => {
        args.forEach(arg => {
          MonoTy.freeTypeVars(arg, fvs)
        });

        return fvs;
      },
      Fun: ({ args, ret }) => {
        args.forEach(arg => {
          MonoTy.freeTypeVars(arg, fvs);
        });

        MonoTy.freeTypeVars(ret, fvs);

        return fvs;
      },
      Tuple: ({ tuple }) => {
        for (const ty of TupleMono.iter(tuple)) {
          MonoTy.freeTypeVars(ty, fvs);
        }

        return fvs;
      },
      Record: ({ row }) => {
        RowMono.fields(row).forEach(([_, ty]) => {
          MonoTy.freeTypeVars(ty, fvs);
        });

        return fvs;
      },
      NamedRecord: ({ row: fields }) => {
        RowMono.fields(fields).forEach(([_, ty]) => {
          MonoTy.freeTypeVars(ty, fvs);
        });

        return fvs;
      },
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
  fresh: () => MonoTy.Var(TyVar.Unbound(Context.freshTyVarIndex())),
  occurs: (x: TyVarId, t: MonoTy): boolean => matchVariant(t, {
    Var: ({ value }) => {
      if (value.kind === 'Unbound') {
        return value.id === x;
      } else {
        return MonoTy.occurs(x, value.to);
      }
    },
    Const: t => t.args.some(a => MonoTy.occurs(x, a)),
    Fun: t => t.args.some(a => MonoTy.occurs(x, a)) || MonoTy.occurs(x, t.ret),
    Tuple: t => TupleMono.toArray(t.tuple).some(a => MonoTy.occurs(x, a)),
    Record: ({ row }) => RowMono.fields(row).some(([_, ty]) => MonoTy.occurs(x, ty)),
    NamedRecord: ({ row: fields }) => RowMono.fields(fields).some(([_, ty]) => MonoTy.occurs(x, ty)),
  }),
  deref: (ty: MonoTy): MonoTy => {
    if (ty.variant === 'Var' && ty.value.kind === 'Link') {
      const ret = MonoTy.deref(ty.value.to);
      ty.value.to = ret;
      return ret;
    }

    return ty;
  },
  substitute: (ty: MonoTy, subst: Map<TyVarId, MonoTy>): MonoTy => {
    const substituteRow = (row: RowMono): RowMono => {
      if (row.type === 'empty') {
        return RowMono.empty();
      }

      return RowMono.extend(row.field,
        MonoTy.substitute(row.ty, subst),
        MonoTy.substitute(row.tail, subst)
      );
    };

    return matchVariant(ty, {
      Var: ({ value }) => matchVariant(value, {
        Unbound: ({ id }) => subst.has(id) ? subst.get(id)! : ty,
        Link: ({ to }) => MonoTy.substitute(to, subst),
      }, 'kind'),
      Const: ({ name, args }) => MonoTy.Const(
        name,
        ...args.map(arg => MonoTy.substitute(arg, subst))
      ),
      Fun: ({ args, ret }) => MonoTy.Fun(
        args.map(arg => MonoTy.substitute(arg, subst)),
        MonoTy.substitute(ret, subst)
      ),
      Tuple: ({ tuple }) => MonoTy.Tuple(
        TupleMono.map(tuple, arg => MonoTy.substitute(arg, subst))
      ),
      Record: ({ row }) => {
        return MonoTy.Record(substituteRow(row));
      },
      NamedRecord: ({ name, row: fields }) => MonoTy.NamedRecord(
        name,
        substituteRow(fields)
      ),
    });
  },
  show: (ty: MonoTy): string => matchVariant(ty, {
    Var: ({ value }) => matchVariant(value, {
      Unbound: ({ id }) => showTyVarId(id),
      Link: ({ to }) => MonoTy.show(to),
    }, 'kind'),
    Const: ({ name, args }) => cond(args.length === 0, {
      then: () => name,
      else: () => matchString(name, {
        'tuple': () => `(${joinWith(args, MonoTy.show, ', ')})`,
        _: () => `${name}<${joinWith(args, MonoTy.show, ', ')}>`,
      }),
    }),
    Fun: ({ args, ret }) => {
      const showParens = args.length !== 1 || (
        args[0].variant === 'Record' ||
        args[0].variant === 'Const' && args[0].name === '()'
      );

      return `${parenthesized(joinWith(args, MonoTy.show, ', '), showParens)} -> ${MonoTy.show(ret)}`;
    },
    Tuple: ({ tuple }) => TupleMono.show(tuple, MonoTy.show),
    Record: ({ row }) => {
      if (row.type === 'empty') {
        return '{}';
      }

      return `{ ${joinWith(RowMono.sortedFields(row), ([k, v]) => `${k}: ${MonoTy.show(v)}`, ', ')} }`;
    },
    NamedRecord: ({ name }) => name,
  }),
  eq: (s: MonoTy, t: MonoTy): boolean =>
    match<[MonoTy, MonoTy]>([MonoTy.deref(s), MonoTy.deref(t)])
      .with(
        [
          { variant: 'Var', value: { kind: 'Unbound' } },
          { variant: 'Var', value: { kind: 'Unbound' } },
        ],
        ([{ value: s }, { value: t }]) => s.id === t.id
      )
      .with(
        [{ variant: 'Var' }, { variant: 'Var' }],
        () => MonoTy.eq(s, t),
      )
      .with(
        [{ variant: 'Const' }, { variant: 'Const' }],
        ([s, t]) => {
          if (s.name === t.name && s.args.length === t.args.length) {
            return s.args.every((arg, i) => MonoTy.eq(arg, t.args[i]));
          }

          return false;
        }
      )
      .with(
        [{ variant: 'Fun' }, { variant: 'Fun' }],
        ([s, t]) => {
          if (s.args.length === t.args.length && MonoTy.eq(s.ret, t.ret)) {
            return s.args.every((arg, i) => MonoTy.eq(arg, t.args[i]));
          }

          return false;
        }
      )
      .with([{ variant: 'Record' }, { variant: 'Record' }], ([s, t]) => {
        return RowMono.strictEq(s.row, t.row);
      })
      .with([{ variant: 'Tuple' }, { variant: 'Tuple' }], ([s, t]) => {
        return TupleMono.eq(s.tuple, t.tuple);
      })
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
    return `<${quantifiedVars}>(${MonoTy.show(monoTy)})`;
  },
  canonicalize: (poly: PolyTy): PolyTy => {
    const [vars, mono] = poly;
    const subst = new Map<TyVarId, MonoTy>();

    const newVars = gen(vars.length, i => i);

    for (const [v1, v2] of zip(vars, newVars)) {
      subst.set(v1, MonoTy.Var({ kind: 'Unbound', id: v2 }));
    }

    return [newVars, MonoTy.substitute(mono, subst)];
  },
  eq: (s: PolyTy, t: PolyTy): boolean => {
    return PolyTy.show(PolyTy.canonicalize(s)) === PolyTy.show(PolyTy.canonicalize(t));
  },
};

export type TypeParams = string[];

export const TypeParams = {
  show: (params: TypeParams) => params.length > 0 ? `<${params.join(', ')}>` : '',
};

export type TypeParamsContext = {
  typeParams: string[],
};

export const TypeParamsContext = {
  make: (): TypeParamsContext => ({ typeParams: [] }),
  clone: (ctx: TypeParamsContext): TypeParamsContext => ({ ...ctx }),
  declare: (ctx: TypeParamsContext, ...names: string[]): void => {
    ctx.typeParams.push(...names);
  },
  has: (ctx: TypeParamsContext, name: string): boolean => {
    return ctx.typeParams.some(p => p === name);
  },
};

export type ParameterizedTy = DataType<{
  Var: { id: TyVarId },
  Param: { name: string },
  Const: { name: string, args: ParameterizedTy[] },
  Fun: { args: ParameterizedTy[], ret: ParameterizedTy },
  Tuple: { tuple: TupleGeneric },
  Record: { row: Row<ParameterizedTy> },
}>;

export const ParameterizedTy = {
  Var: (id: TyVarId): ParameterizedTy => ({ variant: 'Var', id }),
  Param: (name: string): ParameterizedTy => ({ variant: 'Param', name }),
  Const: (name: string, ...args: ParameterizedTy[]): ParameterizedTy => ({ variant: 'Const', name, args }),
  Fun: (args: ParameterizedTy[], ret: ParameterizedTy): ParameterizedTy => ({ variant: 'Fun', args, ret }),
  Tuple: (tuple: TupleGeneric): ParameterizedTy => ({ variant: 'Tuple', tuple }),
  Record: (row: Row<ParameterizedTy>): ParameterizedTy => ({ variant: 'Record', row }),
  fresh: () => ParameterizedTy.Var(Context.freshTyVarIndex()),
  freeTyParams: (ty: ParameterizedTy): string[] => {
    return matchVariant(ty, {
      Var: () => [],
      Param: ({ name }) => [name],
      Const: ({ args }) => args.flatMap(ParameterizedTy.freeTyParams),
      Fun: ({ args, ret }) => [...args.flatMap(ParameterizedTy.freeTyParams), ...ParameterizedTy.freeTyParams(ret)],
      Tuple: ({ tuple }) => TupleGeneric.toArray(tuple).flatMap(ParameterizedTy.freeTyParams),
      Record: ({ row }) => RowGeneric.fields(row).flatMap(([_, ty]) => ParameterizedTy.freeTyParams(ty)),
    });
  },
  substituteTyParams: (ty: ParameterizedTy, subst: Map<string, MonoTy>): MonoTy => {
    return matchVariant(ty, {
      Var: ({ id }) => MonoTy.Var({ kind: 'Unbound', id }),
      Param: ({ name }) => {
        if (!subst.has(name)) {
          panic(`Type parameter '${name}' not found in substitution`);
        }

        return subst.get(name)!;
      },
      Const: ({ name, args }) => MonoTy.Const(name, ...args.map(a => ParameterizedTy.substituteTyParams(a, subst))),
      Fun: ({ args, ret }) => MonoTy.Fun(
        args.map(a => ParameterizedTy.substituteTyParams(a, subst)),
        ParameterizedTy.substituteTyParams(ret, subst)
      ),
      Tuple: ({ tuple }) => MonoTy.Tuple(
        TupleGeneric.map(tuple, a => ParameterizedTy.substituteTyParams(a, subst))
      ),
      Record: ({ row }) => MonoTy.Record(
        RowMono.fromFields(RowGeneric.fields(row).map(([name, ty]) => [name, ParameterizedTy.substituteTyParams(ty, subst)]))
      ),
    });
  },
  toPoly: (ty: ParameterizedTy, params: TypeParams): PolyTy => {
    const subst = new Map<string, MonoTy>();
    const tyVars: TyVarId[] = [];

    for (const t of params) {
      const v = Context.freshTyVarIndex();
      subst.set(t, MonoTy.Var(TyVar.Unbound(v)));
      tyVars.push(v);
    }

    return PolyTy.make(tyVars, ParameterizedTy.substituteTyParams(ty, subst));
  },
  instantiate: (ty: ParameterizedTy, ctx: TypeContext): MonoTy => {
    const subst = new Map<string, MonoTy>(Object.entries(mapRecord(ctx.typeParamsEnv, MonoTy.Var)));
    return ParameterizedTy.substituteTyParams(ty, subst);
  },
  isUnparameterized: (ty: ParameterizedTy): boolean => matchVariant(ty, {
    Var: () => true,
    Param: () => false,
    Const: ({ args }) => args.every(ParameterizedTy.isUnparameterized),
    Fun: ({ args, ret }) => args.every(ParameterizedTy.isUnparameterized) && ParameterizedTy.isUnparameterized(ret),
    Tuple: ({ tuple }) => TupleGeneric.toArray(tuple).every(ParameterizedTy.isUnparameterized),
    Record: ({ row }) => RowGeneric.fields(row).every(([_, ty]) => ParameterizedTy.isUnparameterized(ty)),
  }),
  show: (ty: ParameterizedTy): string => matchVariant(ty, {
    Param: ({ name }) => `'${name}`,
    Var: ({ id }) => showTyVarId(id),
    Const: ({ name, args }) => `${name}${TypeParams.show(args.map(ParameterizedTy.show))}`,
    Fun: ({ args, ret }) => `(${args.map(ParameterizedTy.show).join(', ')}) -> ${ParameterizedTy.show(ret)}`,
    Tuple: ({ tuple }) => TupleGeneric.show(tuple, ParameterizedTy.show),
    Record: ({ row }) => `{ ${RowGeneric.sortedFields(row).map(([name, ty]) => `${name}: ${ParameterizedTy.show(ty)}`).join(', ')} }`,
  }),
  eq: (s: ParameterizedTy, t: ParameterizedTy): boolean => {
    return ParameterizedTy.show(s) === ParameterizedTy.show(t);
  }
};