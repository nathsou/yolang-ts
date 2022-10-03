import { DataType, match, matchMany } from "itsamatch";
import { Context } from "../ast/context";
import { filter, gen, joinWith, sum, zip } from "../utils/array";
import { Maybe } from "../utils/maybe";
import { cond, matchString, panic, parenthesized } from "../utils/misc";
import { diffSet } from "../utils/set";
import { Env } from "./env";
import { Row } from "./structs";
import { Tuple } from "./tuples";
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
  Param: { name: string },
  Const: { name: string, args: MonoTy[] },
  Fun: { args: MonoTy[], ret: MonoTy },
  Tuple: { tuple: Tuple },
  Struct: { name?: string, params: MonoTy[], row: Row },
  Union: { tys: MonoTy[] },
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
  Array: (elemTy: MonoTy): MonoTy => MonoTy.Const('Array', elemTy),
  Tuple: (tuple: Tuple): MonoTy => ({ variant: 'Tuple', tuple }),
  Struct: (fields: Readonly<Row>, name?: string, params: MonoTy[] = []): MonoTy => ({ variant: 'Struct', name, params, row: fields }),
  Union: (tys: MonoTy[]): MonoTy => {
    const flat: MonoTy[] = [];

    // merge union types
    const addTy = (ty: MonoTy) => {
      if (ty.variant === 'Union') {
        ty.tys.forEach(addTy);
      } else {
        flat.push(ty);
      }
    };

    tys.forEach(addTy);

    return { variant: 'Union', tys: flat };
  },
  toPoly: (ty: MonoTy): PolyTy => [[], ty],
  ptr: (ty: MonoTy): MonoTy => MonoTy.Const('ptr', ty),
  int: () => MonoTy.Union([MonoTy.u8(), MonoTy.i8(), MonoTy.u32(), MonoTy.i32(), MonoTy.u64(), MonoTy.i64()]),
  u32: () => MonoTy.Const('u32'),
  i32: () => MonoTy.Const('i32'),
  u64: () => MonoTy.Const('u64'),
  i64: () => MonoTy.Const('i64'),
  i8: () => MonoTy.Const('i8'),
  u8: () => MonoTy.Const('u8'),
  bool: () => MonoTy.Const('bool'),
  str: () => MonoTy.Const('str'), // not primitive
  void: () => MonoTy.Const('void'),
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
      Union: ({ tys }) => {
        for (const ty of tys) {
          MonoTy.freeTypeVars(ty, fvs);
        }

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
    Union: ({ tys }) => tys.some(a => MonoTy.occurs(x, a)),
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
      Union: ({ tys }) => MonoTy.Union(tys.map(ty => MonoTy.substitute(ty, subst))),
    });
  },
  substituteTyParams: (ty: MonoTy, subst: Map<string, MonoTy>): MonoTy => {
    return MonoTy.rewrite(ty, t => {
      if (t.variant === 'Param') {
        if (!subst.has(t.name)) {
          panic(`Type parameter '${t.name}' not found in substitution`);
        }

        return subst.get(t.name)!;
      }

      return t;
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
      else: () => matchString(name, {
        'Array': () => `${MonoTy.show(args[0])}[]`,
        _: () => `${name}<${joinWith(args, MonoTy.show, ', ')}>`,
      }),
    }),
    Fun: ({ args, ret }) => {
      const showParens = args.length !== 1 || (
        args[0].variant === 'Struct'
      );

      return `${parenthesized(joinWith(args, MonoTy.show, ', '), showParens)} -> ${MonoTy.show(ret)}`;
    },
    Tuple: ({ tuple }) => Tuple.show(tuple),
    Struct: ({ row, name, params }) => {
      // if (name != null) {
      //   if (params.length > 0) {
      //     return `${name}<${params.map(MonoTy.show).join(', ')}>`;
      //   } else {
      //     return name;
      //   }
      // }

      if (row.type === 'empty') {
        return '{}';
      }

      return `{ ${joinWith(Row.sortedFields(row), ([k, v]) => `${k}: ${MonoTy.show(v)}`, ', ')} }`;
    },
    Union: ({ tys }) => joinWith(tys, MonoTy.show, ' | '),
  }),
  eq: (s: MonoTy, t: MonoTy): boolean => matchMany([MonoTy.deref(s), MonoTy.deref(t)], {
    'Var Var': ({ value: v1 }, { value: v2 }) => {
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
  rewrite: (ty: MonoTy, f: (ty: MonoTy) => MonoTy): MonoTy => {
    const go = (t: MonoTy) => MonoTy.rewrite(t, f);
    return f(match(ty, {
      Const: ({ name, args }) => MonoTy.Const(name, ...args.map(go)),
      Fun: ({ args, ret }) => MonoTy.Fun(args.map(go), go(ret)),
      Struct: ({ name, params, row }) => MonoTy.Struct(Row.map(row, go), name, params),
      Tuple: ({ tuple }) => MonoTy.Tuple(Tuple.fromArray(Tuple.toArray(tuple).map(go))),
      Union: ({ tys }) => MonoTy.Union(tys.map(go)),
      Param: p => p,
      Var: v => v,
    }));
  },
  expand: (ty: MonoTy, ctx: TypeContext): MonoTy => MonoTy.rewrite(ty, t => {
    if (t.variant === 'Const' && ctx.typeAliases.has(t.name)) {
      return TypeContext.instantiateTypeAlias(ctx, ctx.typeAliases.get(t.name)!, t.args);
    } else if (t.variant === 'Param' && ctx.typeParamsEnv.has(t.name)) {
      return ctx.typeParamsEnv.get(t.name)!;
    } else if (t.variant === 'Union' && t.tys.length === 1) {
      return t.tys[0];
    }

    return t;
  }),
  specificity: (ty: MonoTy): number => match(ty, {
    Param: () => 1,
    Var: () => 2,
    Const: () => 4,
    Fun: ({ args, ret }) => sum(args.map(MonoTy.specificity)) + MonoTy.specificity(ret),
    Struct: ({ row }) => sum(Row.fields(row).map(([_, ty]) => MonoTy.specificity(ty))),
    Tuple: ({ tuple }) => sum(Tuple.toArray(tuple).map(MonoTy.specificity)),
    Union: ({ tys }) => sum(tys.map(MonoTy.specificity)),
  }),
};

export type PolyTy = [TyVarId[], MonoTy];

export const PolyTy = {
  make: (quantifiedVars: TyVarId[], monoTy: MonoTy): PolyTy => [quantifiedVars, monoTy],
  fresh: () => MonoTy.toPoly(MonoTy.fresh()),
  from: (tyParams: string[], ty: MonoTy): PolyTy => {
    const quantified = tyParams.map(Context.freshTyVarIndex);
    const subst = new Map(zip(tyParams, quantified.map(id => MonoTy.Var(TyVar.Unbound(id)))));
    return PolyTy.make(quantified, MonoTy.substituteTyParams(ty, subst));
  },
  instantiate: ([quantifiedVars, ty]: PolyTy) => {
    // replace all bound type variables with fresh type variables
    const subst = new Map<TyVarId, MonoTy>();
    quantifiedVars.forEach(id => {
      subst.set(id, MonoTy.fresh());
    });

    const freeVars = MonoTy.freeTypeVars(ty);
    const inst = MonoTy.substitute(ty, subst);

    return { ty: inst, subst: new Map(filter(subst, ([id]) => freeVars.has(id))) };
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
    Const: ({ args }) => args.every(a => PolyTy.isDetermined([vars, a])),
    Fun: ({ args, ret }) => args.every(arg => PolyTy.isDetermined([vars, arg])) && PolyTy.isDetermined([vars, ret]),
    Tuple: ({ tuple }) => Tuple.toArray(tuple).every(ty => PolyTy.isDetermined([vars, ty])),
    Struct: ({ row }) => Row.fields(row).every(([, ty]) => PolyTy.isDetermined([vars, ty])),
    Union: ({ tys }) => tys.every(ty => PolyTy.isDetermined([vars, ty])),
    Param: () => true,
  }),
  isPolymorphic: ([quantified, _]: PolyTy): boolean => quantified.length > 0,
};

export type TypeParam = { name: string, constraints: MonoTy[] };

export const TypeParams = {
  show: (params: TypeParam[]) => params.length > 0 ? `<${joinWith(params, p => p.constraints.length === 0 ? p.name : `${p.name}: ${joinWith(p.constraints, MonoTy.show, ' + ')}`)}>` : '',
  hash: (params: MonoTy[]) => `<${joinWith(params, MonoTy.show)}>`,
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
