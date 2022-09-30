import { DataType, genConstructors, match, VariantOf } from "itsamatch";
import { Error } from "../errors/errors";
import { Row } from "../infer/structs";
import { TypeContext } from "../infer/typeContext";
import { MonoTy, PolyTy, TypeParams } from "../infer/types";
import { Const } from "../parse/token";
import { zip } from "../utils/array";
import { Maybe, none } from "../utils/maybe";
import { assert, block, panic, proj, pushMap } from "../utils/misc";
import * as bitter from './bitter';
import type { Mono } from "./monomorphize";
import { FuncName, VarName } from "./name";

type Typed<T> = {
  [K in keyof T]: T[K] & { ty: MonoTy }
};

export type ArrayInit = DataType<{
  elems: { elems: Expr[] },
  fill: { value: Expr, count: number },
}>;

export const ArrayInit = {
  ...genConstructors<ArrayInit>(['elems', 'fill']),
  len: (init: ArrayInit): number => match(init, {
    elems: ({ elems }) => elems.length,
    fill: ({ count }) => count,
  }),
};

function resolveNamedStruct(name: string, typeParams: MonoTy[], ctx: TypeContext): Maybe<VariantOf<MonoTy, 'Struct'>> {
  const ta = TypeContext.resolveTypeAlias(ctx, name);

  return ta.map(({ ty, params }) => {
    assert(ty.variant === 'Struct');
    const subst = new Map(zip(params.map(proj('name')), typeParams));
    const struct = MonoTy.substituteTyParams(ty, subst) as VariantOf<MonoTy, 'Struct'>;
    assert(struct.variant === 'Struct');

    return struct;
  });
}

function resolveAnonymousStruct(ty: VariantOf<MonoTy, 'Struct'>, ctx: TypeContext): VariantOf<MonoTy, 'Struct'> {
  if (ty.name != null) {
    return ty;
  }

  const candidates: { name: string, ty: VariantOf<MonoTy, 'Struct'> }[] = [];

  for (const [name, ta] of ctx.typeAliases) {
    if (ta.ty.variant === 'Struct' && Row.strictEq(ta.ty.row, ty.row)) {
      ta.ty.name = name;
      candidates.push({ name, ty: ta.ty });
    }
  }

  assert(candidates.length !== 0, () => `unknown struct type: ${MonoTy.show(ty)}`);
  assert(candidates.length < 2, () => `ambiguous struct type: ${MonoTy.show(ty)}`);

  return candidates[0].ty;
}

export type Expr = DataType<Typed<{
  Const: { value: Const },
  Variable: { name: VarName },
  NamedFuncCall: { name: FuncName, args: Expr[] },
  IfThenElse: { condition: Expr, then: Expr, else_: Maybe<Expr> },
  StructAccess: { struct: VariantOf<MonoTy, 'Struct'>, lhs: Expr, field: string },
  Struct: { structTy: VariantOf<MonoTy, 'Struct'>, fields: { name: string, value: Expr }[] },
  Block: { stmts: Stmt[], ret: Maybe<Expr> },
  // TODO: replace with core structs, assignments and loops
  Array: { init: ArrayInit, elemTy: MonoTy },
}>>;

export const Expr = {
  ...genConstructors<Expr>([
    'Const', 'Variable', 'NamedFuncCall', 'IfThenElse',
    'StructAccess', 'Struct', 'Block', 'Array',
  ]),
  from: (expr: bitter.Expr, f: VariantOf<Decl, 'Function'>, ctx: Mono['Context']): Expr => {
    const go = (expr: bitter.Expr) => Expr.from(expr, f, ctx);
    return match(expr, {
      Const: ({ value, ty }) => Expr.Const({ value, ty }),
      Variable: ({ name, ty }) => Expr.Variable({ name, ty }),
      NamedFuncCall: ({ name, typeParams, args, ty }) => {
        const resolvedName = block<FuncName>(() => {
          const overloadName = name.unwrapRight('core.Expr.from: NamedFuncCall');
          if (typeParams.length > 0) {
            const key = TypeParams.hash(typeParams);
            assert(ctx.instances.has(overloadName.mangled), () => `missing instance '${overloadName.mangled}'`);
            const instances = ctx.instances.get(overloadName.mangled)!;
            assert(instances.has(key), () => `missing instance '${overloadName.mangled}${key}'`);
            return instances.get(key)!.fun.name;
          } else {
            return overloadName;
          }
        });

        return Expr.NamedFuncCall({
          name: resolvedName,
          args: args.map(go),
          ty,
        });
      },
      Block: ({ statements, lastExpr, ty }) => Expr.Block({
        stmts: statements.map(stmt => Stmt.from(stmt, f, ctx)),
        ret: lastExpr.map(go),
        ty,
      }),
      IfThenElse: ({ condition, then, else_, ty }) => Expr.IfThenElse({
        condition: go(condition),
        then: go(then),
        else_: else_.map(go),
        ty
      }),
      Struct: ({ name, typeParams, fields, ty }) => Expr.Struct({
        structTy: resolveNamedStruct(name, typeParams, ctx.types).unwrap('resolveStruct'),
        fields: fields.map(({ name, value }) =>
          ({ name, value: go(value) })),
        ty
      }),
      FieldAccess: ({ lhs, field, ty }) => {
        const structTy = MonoTy.deref(lhs.ty);
        assert(structTy.variant === 'Struct');
        return Expr.StructAccess({
          lhs: go(lhs),
          struct: resolveAnonymousStruct(structTy, ctx.types),
          field,
          ty,
        });
      },
      Array: ({ elemTy, init, ty }) => Expr.Array({
        elemTy,
        init: match(init, {
          elems: ({ elems }) => ArrayInit.elems({ elems: elems.map(go) }),
          fill: ({ count, value }) => ArrayInit.fill({ count, value: go(value) }),
        }),
        ty
      }),
      _: () => panic('Unsupported core expression: ' + expr.variant),
    });
  },
};

export type Stmt = DataType<{
  Let: { mut: boolean, name: VarName, value: Expr },
  VariableAssignment: { name: VarName, value: Expr },
  StructAssignment: { struct: Expr, structTy: VariantOf<MonoTy, 'Struct'>, field: string, value: Expr },
  Expr: { expr: Expr },
  While: { condition: Expr, statements: Stmt[] },
  Return: { expr: Maybe<Expr> },
}>;

export const Stmt = {
  ...genConstructors<Stmt>(['Let', 'VariableAssignment', 'StructAssignment', 'Expr', 'While', 'Return']),
  from: (stmt: bitter.Stmt, f: VariantOf<Decl, 'Function'>, ctx: Mono['Context']): Stmt => match(stmt, {
    Let: ({ mutable, name, expr }) => Stmt.Let({ mut: mutable, name, value: Expr.from(expr, f, ctx) }),
    Expr: ({ expr }) => Stmt.Expr({ expr: Expr.from(expr, f, ctx) }),
    While: ({ condition, statements }) => Stmt.While({
      condition: Expr.from(condition, f, ctx),
      statements: statements.map(stmt => Stmt.from(stmt, f, ctx))
    }),
    Assignment: ({ lhs, rhs }) => match(lhs, {
      Variable: ({ name }) => Stmt.VariableAssignment({ name, value: Expr.from(rhs, f, ctx) }),
      FieldAccess: ({ lhs, field }) => {
        const structTy = MonoTy.deref(lhs.ty);
        assert(structTy.variant === 'Struct');
        return Stmt.StructAssignment({ struct: Expr.from(lhs, f, ctx), structTy, field, value: Expr.from(rhs, f, ctx) });
      },
      _: () => panic('Unsupported assignment target: ' + lhs.variant),
    }),
    Return: ({ expr }) => {
      f.canReturnEarly ||= true;
      return Stmt.Return({ expr: expr.map(e => Expr.from(e, f, ctx)) });
    },
    _: () => panic('Unhandled core statement: ' + stmt.variant),
  }),
};

export type Decl = DataType<{
  Function: {
    attributes: Map<string, { args: string[] }>,
    pub: boolean,
    name: FuncName,
    args: { mut: boolean, name: VarName }[],
    ty: VariantOf<MonoTy, 'Fun'>,
    body: Maybe<Expr>,
    returnTy: MonoTy,
    canReturnEarly: boolean, // the function body contains at least one early return statement
  },
}>;

export const Decl = {
  ...genConstructors<Decl>(['Function']),
  from: (decl: bitter.Decl, ctx: Mono['Context']): Decl[] => match(decl, {
    Function: func => {
      const { attributes, pub, name, args, body, funTy } = func;

      if (PolyTy.isPolymorphic(funTy)) {
        return [];
      }

      const ty = MonoTy.deref(funTy[1]);
      assert(ty.variant === 'Fun');

      const f = Decl.Function({
        attributes: new Map(attributes.map(({ name, args }) => [name, { args }])),
        pub,
        name,
        args: args.map(({ mutable, name }) => ({ mut: mutable, name })),
        ty,
        body: none,
        returnTy: MonoTy.deref(ty.ret),
        canReturnEarly: false,
      });

      f.body = body.map(e => Expr.from(e, f, ctx));

      return [f];
    },
    TypeAlias: () => [],
    Import: () => [],
    _: () => panic('Unhandled core declaration: ' + decl.variant),
  }),
};

export type Module = {
  name: string,
  path: string,
  decls: Decl[],
  members: Map<string, VariantOf<Decl, 'Function'>[]>,
  imports: Map<string, { sourceMod: string, isExport: boolean }>,
  typeContext: TypeContext,
};

export const Module = {
  from: (mod: bitter.Module, instances: Mono['Instances']): Module => {
    const ctx: Mono['Context'] = { types: mod.typeContext, instances };
    const decls = mod.decls.flatMap(d => Decl.from(d, ctx));
    const coreMod: Module = {
      name: mod.name,
      path: mod.path,
      decls,
      imports: mod.imports,
      members: new Map(),
      typeContext: mod.typeContext,
    };

    for (const decl of decls) {
      match(decl, {
        Function: f => {
          pushMap(coreMod.members, f.name.original, f);
        },
        _: () => { },
      });
    }

    return coreMod;
  }
};

export type Prog = {
  modules: Map<string, Module>,
  entry: Module,
};

export const Prog = {
  from: (monoProg: bitter.Prog, instances: Mono['Instances']): [Prog, Error[]] => {
    const errors: Error[] = [];
    const coreModules = new Map<string, Module>();

    for (const [path, mod] of monoProg.modules) {
      coreModules.set(path, Module.from(mod, instances));
    }

    return [{
      modules: coreModules,
      entry: coreModules.get(monoProg.entry.path)!,
    }, errors];
  },
};
