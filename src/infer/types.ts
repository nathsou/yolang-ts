import { DataType, match, matchMany } from "itsamatch";
import { Context } from "../ast/context";
import { gen, joinWith, zip } from "../utils/array";
import { Maybe } from "../utils/maybe";
import { cond, panic, parenthesized, proj } from "../utils/misc";
import { diffSet } from "../utils/set";
import { Env } from "./env";
import { Row } from "./structs";
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
  Const: { name: string, args: MonoTy[] },
  Fun: { args: MonoTy[], ret: MonoTy },
  Tuple: { tuple: Tuple },
  Struct: { name?: string, row: Row },
}>;

export const showTyVarId = (n: TyVarId): string => {
  const l = String.fromCharCode(65 + n % 26);
  return n >= 26 ? `${l}${Math.floor(n / 26)}` : l;
};

export const MonoTy = {
  Var: (tv: TyVar) => match(tv, {
    Unbound: (tv): MonoTy => ({ variant: 'Var', value: tv }),
    Link: ({ to }): MonoTy => ({ variant: 'Var', value: { kind: 'Link', to: MonoTy.deref(to) } }),
  }, 'kind'),
  Param: (name: string): MonoTy => ({ variant: 'Param', name }),
  Const: (name: string, ...args: MonoTy[]): MonoTy => ({ variant: 'Const', name, args }),
  Fun: (args: MonoTy[], ret: MonoTy): MonoTy => ({ variant: 'Fun', args, ret }),
  Tuple: (tuple: Tuple): MonoTy => ({ variant: 'Tuple', tuple }),
  Struct: (fields: Readonly<Row>, name?: string): MonoTy => ({ variant: 'Struct', name, row: fields }),
  toPoly: (ty: MonoTy): PolyTy => [[], ty],
  u32: () => MonoTy.Const('u32'),
  i32: () => MonoTy.Const('i32'),
  u64: () => MonoTy.Const('u64'),
  i64: () => MonoTy.Const('i64'),
  bool: () => MonoTy.Const('bool'),
  unit: () => MonoTy.Const('()'),
  primitiveTypes: new Set(['u32', 'i32', 'u64', 'i64', 'bool', '()']),
  isPrimitive: (ty: MonoTy): boolean => ty.variant === 'Const' && MonoTy.primitiveTypes.has(ty.name),
  freeTypeVars: (ty: MonoTy, fvs: Set<TyVarId> = new Set()): Set<TyVarId> =>
    match(ty, {
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
      Struct: ({ row }) => {
        Row.fields(row).forEach(([_, ty]) => {
          MonoTy.freeTypeVars(ty, fvs);
        });

        return fvs;
      },
      Integer: () => fvs,
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
  occurs: (x: TyVarId, t: MonoTy): boolean => match(t, {
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
    Struct: ({ row: fields }) => Row.fields(fields).some(([_, ty]) => MonoTy.occurs(x, ty)),
    Integer: () => false,
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

    return match(ty, {
      Var: ({ value }) => match(value, {
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
      Struct: ({ name, row: fields }) => MonoTy.Struct(
        substituteRow(fields),
        name,
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

    return match(ty, {
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
      Struct: ({ name, row }) => MonoTy.Struct(
        substRowTyParams(row),
        name,
      ),
    });
  },
  instantiateTyParams: (ty: MonoTy, tyParams: string[]): MonoTy => {
    const subst = new Map(zip(tyParams, tyParams.map(MonoTy.fresh)));
    return MonoTy.substituteTyParams(ty, subst);
  },
  show: (ty: MonoTy): string => match(ty, {
    Var: ({ value }) => match(value, {
      Unbound: ({ id }) => showTyVarId(id),
      Link: ({ to }) => MonoTy.show(to),
    }, 'kind'),
    Param: ({ name }) => `?${name}`,
    Const: ({ name, args }) => cond(args.length === 0, {
      then: () => name,
      else: () => `${name}<${joinWith(args, MonoTy.show, ', ')}>`,
    }),
    Fun: ({ args, ret }) => {
      const showParens = args.length !== 1 || (
        args[0].variant === 'Struct' ||
        args[0].variant === 'Const' && args[0].name === '()'
      );

      return `${parenthesized(joinWith(args, MonoTy.show, ', '), showParens)} -> ${MonoTy.show(ret)}`;
    },
    Tuple: ({ tuple }) => Tuple.show(tuple),
    Struct: ({ row, name }) => {
      if (name != null) {
        return name;
      }

      if (row.type === 'empty') {
        return '{}';
      }

      return `{ ${joinWith(Row.sortedFields(row), ([k, v]) => `${k}: ${MonoTy.show(v)}`, ', ')} }`;
    },
  }),
  eq: (s: MonoTy, t: MonoTy): boolean => matchMany([MonoTy.deref(s), MonoTy.deref(t)], {
    "Var Var": ({ value: v1 }, { value: v2 }) => {
      if (v1.kind === 'Unbound' && v2.kind === 'Unbound') {
        return v1.id === v2.id;
      }

      if (v1.kind === 'Link' && v2.kind === 'Link') {
        return MonoTy.eq(v1.to, v2.to);
      }

      return false;
    },
    'Const Const': (c1, c2) => {
      if (
        c1.name === c2.name &&
        c1.args.length === c2.args.length
      ) {
        return zip(c1.args, c2.args).every(([s, t]) => MonoTy.eq(s, t));
      }

      return false;
    },
    'Fun Fun': (f, g) => {
      if (f.args.length === g.args.length && MonoTy.eq(f.ret, g.ret)) {
        return zip(f.args, g.args).every(([s, t]) => MonoTy.eq(s, t));
      }

      return false;
    },
    'Struct Struct': (r1, r2) => Row.strictEq(r1.row, r2.row),
    'Tuple Tuple': (t1, t2) => Tuple.eq(t1.tuple, t2.tuple),
    'Param Param': (p1, p2) => p1.name === p2.name,
    _: () => false,
  }),
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
  typeVars: ([quantified, _]: PolyTy) => quantified,
  freeTypeVars: ([quantified, monoTy]: PolyTy): Set<TyVarId> => {
    const freeVarsMonoTy = MonoTy.freeTypeVars(monoTy);
    return diffSet(freeVarsMonoTy, new Set(quantified));
  },
  show: (ty: PolyTy, canonical = true): string => {
    const [quantified, monoTy] = canonical ? PolyTy.canonicalize(ty) : ty;

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
  isDetermined: ([vars, monoTy]: PolyTy): boolean => match(monoTy, {
    Var: ({ value }) => match(value, {
      Unbound: ({ id }) => vars.includes(id),
      Link: ({ to }) => PolyTy.isDetermined([vars, to]),
    }, 'kind'),
    Const: () => true,
    Fun: ({ args, ret }) => args.every(arg => PolyTy.isDetermined([vars, arg])) && PolyTy.isDetermined([vars, ret]),
    Tuple: ({ tuple }) => Tuple.toArray(tuple).every(t => PolyTy.isDetermined([vars, t])),
    Struct: ({ row }) => Row.fields(row).every(([, ty]) => PolyTy.isDetermined([vars, ty])),
    Param: () => true,
    Integer: () => true,
  }),
  isPolymorphic: ([quantified, _]: PolyTy): boolean => quantified.length > 0,
};

export type TypeParam = { name: string, ty: Maybe<MonoTy> };

export const TypeParams = {
  show: (params: TypeParam[]) => params.length > 0 ? `<${params.map(proj('name')).join(', ')}>` : '',
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