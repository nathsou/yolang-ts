import { DataType, match as matchVariant } from "itsamatch";
import { match } from "ts-pattern";
import { Context } from "../ast/context";
import { gen, joinWith, zip } from "../utils/array";
import { cond, id, panic, parenthesized } from "../utils/misc";
import { diffSet } from "../utils/set";
import { Env } from "./env";
import { Row } from "./records";
import { Tuple } from "./tuples";

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
  Param: { name: string },
  Const: { path: string[], name: string, args: MonoTy[] },
  Fun: { args: MonoTy[], ret: MonoTy },
  Tuple: { tuple: Tuple },
  Record: { row: Row },
  NamedRecord: { name: string, row: Row },
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
  Param: (name: string): MonoTy => ({ variant: 'Param', name }),
  Const: (name: string, ...args: MonoTy[]): MonoTy => ({ variant: 'Const', path: [], name, args }),
  ConstWithPath: (path: string[], name: string, ...args: MonoTy[]): MonoTy => ({ variant: 'Const', path, name, args }),
  Fun: (args: MonoTy[], ret: MonoTy): MonoTy => ({ variant: 'Fun', args, ret }),
  Tuple: (tuple: Tuple): MonoTy => ({ variant: 'Tuple', tuple }),
  Record: (row: Row): MonoTy => ({ variant: 'Record', row }),
  NamedRecord: (name: string, fields: Readonly<Row>): MonoTy => ({ variant: 'NamedRecord', name, row: fields }),
  toPoly: (ty: MonoTy): PolyTy => [[], ty],
  u32: () => MonoTy.Const('u32'),
  bool: () => MonoTy.Const('bool'),
  unit: () => MonoTy.Const('()'),
  primitiveTypes: new Set(['u32', 'bool', '()']),
  isPrimitive: (ty: MonoTy): boolean => ty.variant === 'Const' && ty.path.length === 0 && MonoTy.primitiveTypes.has(ty.name),
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
      Param: () => fvs,
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
        for (const ty of Tuple.iter(tuple)) {
          MonoTy.freeTypeVars(ty, fvs);
        }

        return fvs;
      },
      Record: ({ row }) => {
        Row.fields(row).forEach(([_, ty]) => {
          MonoTy.freeTypeVars(ty, fvs);
        });

        return fvs;
      },
      NamedRecord: ({ row: fields }) => {
        Row.fields(fields).forEach(([_, ty]) => {
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
    Param: () => false,
    Const: t => t.args.some(a => MonoTy.occurs(x, a)),
    Fun: t => t.args.some(a => MonoTy.occurs(x, a)) || MonoTy.occurs(x, t.ret),
    Tuple: t => Tuple.toArray(t.tuple).some(a => MonoTy.occurs(x, a)),
    Record: ({ row }) => Row.fields(row).some(([_, ty]) => MonoTy.occurs(x, ty)),
    NamedRecord: ({ row: fields }) => Row.fields(fields).some(([_, ty]) => MonoTy.occurs(x, ty)),
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
    const substituteRow = (row: Row): Row => {
      if (row.type === 'empty') {
        return Row.empty();
      }

      return Row.extend(row.field,
        MonoTy.substitute(row.ty, subst),
        MonoTy.substitute(row.tail, subst)
      );
    };

    return matchVariant(ty, {
      Var: ({ value }) => matchVariant(value, {
        Unbound: ({ id }) => subst.has(id) ? subst.get(id)! : ty,
        Link: ({ to }) => MonoTy.substitute(to, subst),
      }, 'kind'),
      Param: () => ty,
      Const: ({ name, args }) => MonoTy.Const(
        name,
        ...args.map(arg => MonoTy.substitute(arg, subst))
      ),
      Fun: ({ args, ret }) => MonoTy.Fun(
        args.map(arg => MonoTy.substitute(arg, subst)),
        MonoTy.substitute(ret, subst)
      ),
      Tuple: ({ tuple }) => MonoTy.Tuple(
        Tuple.map(tuple, arg => MonoTy.substitute(arg, subst))
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
  substituteTyParams: (ty: MonoTy, subst: Map<string, MonoTy>): MonoTy => {
    const substRowTyParams = (row: Row): Row => {
      return Row.fromFields(
        Row.fields(row).map(([name, ty]) =>
          [name, MonoTy.substituteTyParams(ty, subst)]
        ),
        false
      );
    };

    return matchVariant(ty, {
      Var: () => {
        if (ty.variant === 'Var' && ty.value.kind === 'Link') {
          return MonoTy.substituteTyParams(ty.value.to, subst);
        }

        return ty;
      },
      Param: ({ name }) => {
        if (!subst.has(name)) {
          panic(`Type parameter '${name}' not found in substitution`);
        }

        return subst.get(name)!;
      },
      Const: ({ name, args }) => MonoTy.Const(name, ...args.map(a => MonoTy.substituteTyParams(a, subst))),
      Fun: ({ args, ret }) => MonoTy.Fun(
        args.map(a => MonoTy.substituteTyParams(a, subst)),
        MonoTy.substituteTyParams(ret, subst)
      ),
      Tuple: ({ tuple }) => MonoTy.Tuple(
        Tuple.map(tuple, a => MonoTy.substituteTyParams(a, subst))
      ),
      Record: ({ row }) => MonoTy.Record(substRowTyParams(row)),
      NamedRecord: ({ name, row }) => MonoTy.NamedRecord(
        name,
        substRowTyParams(row)
      ),
    });
  },
  instantiateTyParams: (ty: MonoTy, tyParams: string[]): MonoTy => {
    const subst = new Map(zip(tyParams, tyParams.map(MonoTy.fresh)));
    return MonoTy.substituteTyParams(ty, subst);
  },
  show: (ty: MonoTy): string => matchVariant(ty, {
    Var: ({ value }) => matchVariant(value, {
      Unbound: ({ id }) => showTyVarId(id),
      Link: ({ to }) => MonoTy.show(to),
    }, 'kind'),
    Param: ({ name }) => `?${name}`,
    Const: ({ path, name, args }) => (path.length > 0 ? `${path.join('.')}.` : '') + cond(args.length === 0, {
      then: () => name,
      else: () => `${name}<${joinWith(args, MonoTy.show, ', ')}>`,
    }),
    Fun: ({ args, ret }) => {
      const showParens = args.length !== 1 || (
        args[0].variant === 'Record' ||
        args[0].variant === 'Const' && args[0].name === '()'
      );

      return `${parenthesized(joinWith(args, MonoTy.show, ', '), showParens)} -> ${MonoTy.show(ret)}`;
    },
    Tuple: ({ tuple }) => Tuple.show(tuple),
    Record: ({ row }) => {
      if (row.type === 'empty') {
        return '{}';
      }

      return `{ ${joinWith(Row.sortedFields(row), ([k, v]) => `${k}: ${MonoTy.show(v)}`, ', ')} }`;
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
          if (
            s.name === t.name &&
            s.args.length === t.args.length &&
            s.path.length === t.path.length &&
            zip(s.path, t.path).every(([s, t]) => s === t)
          ) {
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
        return Row.strictEq(s.row, t.row);
      })
      .with([{ variant: 'Tuple' }, { variant: 'Tuple' }], ([s, t]) => {
        return Tuple.eq(s.tuple, t.tuple);
      })
      .otherwise(() => false),
  isDetermined: (s: MonoTy): boolean => PolyTy.isDetermined([[], s]),
};

export type PolyTy = [TyVarId[], MonoTy];

export const PolyTy = {
  make: (quantifiedVars: TyVarId[], monoTy: MonoTy): PolyTy => [quantifiedVars, monoTy],
  fresh: () => MonoTy.toPoly(MonoTy.fresh()),
  instantiate: ([quantifiedVars, ty]: PolyTy) => {
    // replace all bound type variables with fresh type variables
    const subst = new Map<TyVarId, MonoTy>();
    quantifiedVars.forEach(id => {
      subst.set(id, MonoTy.fresh());
    });

    return { ty: MonoTy.substitute(ty, subst), subst };
  },
  instantiatePartially: ([quantifiedVars, ty]: PolyTy, inst: MonoTy[]): PolyTy => {
    const subst = new Map<TyVarId, MonoTy>(zip(quantifiedVars, inst));
    const remainingVars = quantifiedVars.slice(inst.length);

    return [remainingVars, MonoTy.substitute(ty, subst)];
  },
  instantiateTyParams: (tyParams: TypeParams, ty: MonoTy): PolyTy => {
    const quantifiedVars = gen(tyParams.length, id);
    const subst = new Map<string, MonoTy>(zip(tyParams, quantifiedVars.map(id => MonoTy.Var(TyVar.Unbound(id)))));

    return [quantifiedVars, MonoTy.substituteTyParams(ty, subst)];
  },
  typeVars: ([quantified, _]: PolyTy) => quantified,
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
  isDetermined: ([vars, monoTy]: PolyTy): boolean => matchVariant(monoTy, {
    Var: ({ value }) => matchVariant(value, {
      Unbound: ({ id }) => vars.includes(id),
      Link: ({ to }) => PolyTy.isDetermined([vars, to]),
    }, 'kind'),
    Const: () => true,
    Fun: ({ args, ret }) => args.every(arg => PolyTy.isDetermined([vars, arg])) && PolyTy.isDetermined([vars, ret]),
    Tuple: ({ tuple }) => Tuple.toArray(tuple).every(t => PolyTy.isDetermined([vars, t])),
    Record: ({ row }) => Row.fields(row).every(([, ty]) => PolyTy.isDetermined([vars, ty])),
    NamedRecord: ({ row }) => Row.fields(row).every(([, ty]) => PolyTy.isDetermined([vars, ty])),
    Param: () => true,
  }),
  isPolymorphic: ([quantified, _]: PolyTy): boolean => quantified.length > 0,
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
  clone: (ctx: TypeParamsContext): TypeParamsContext => ({
    typeParams: [...ctx.typeParams],
  }),
  declare: (ctx: TypeParamsContext, ...names: string[]): void => {
    ctx.typeParams.push(...names);
  },
  has: (ctx: TypeParamsContext, name: string): boolean => {
    return ctx.typeParams.some(p => p === name);
  },
};