import { Error } from "../errors/errors";
import { FunDecl } from "../infer/env";
import { inferDecl } from "../infer/infer";
import { TypeContext } from "../infer/typeContext";
import { MonoTy, PolyTy, showTyVarId, TypeParams } from "../infer/types";
import { zip } from "../utils/array";
import { some } from "../utils/maybe";
import { assert, block, id } from "../utils/misc";
import { Decl } from "./bitter";
import { NameEnv } from "./name";

export type MonoInstances = Map<string, Map<string, FunDecl>>;

export type MonoContext = {
  types: TypeContext,
  instances: MonoInstances,
};

export const monomorphize = (
  ctx: MonoContext,
  f: FunDecl,
  typeParams: MonoTy[],
  errors: Error[]
): FunDecl => {
  const key = TypeParams.hash(typeParams);
  // check if an instance for this overload of f and these type parameters already exists
  if (ctx.instances.has(f.name.mangled) && ctx.instances.get(f.name.mangled)!.has(key)) {
    return ctx.instances.get(f.name.mangled)!.get(key)!;
  }

  const newTyCtx = TypeContext.clone(ctx.types);
  const nameEnv = NameEnv.make();
  const instanceTy = block(() => {
    const instanceTy = PolyTy.instantiatePartially(f.funTy, typeParams);
    assert(!PolyTy.isPolymorphic(instanceTy), 'instantiated generic function is still polymorphic');
    const res = instanceTy[1];
    assert(res.variant === 'Fun');
    // console.log('instantiating', f.name.mangled + PolyTy.show(f.funTy), 'with', key, '==>', MonoTy.show(res));
    return res;
  });

  const inst = Decl.rewrite(f, nameEnv, id) as FunDecl;
  inst.name.mangled += key;
  inst.returnTy = some(instanceTy.ret);
  inst.args = zip(inst.args, instanceTy.args).map(([a, ty]) => ({
    name: a.name,
    mutable: a.mutable,
    annotation: some(ty)
  }));

  inst.typeParams = [];
  typeParams.forEach((p, index) => {
    inst.typeParams.push({
      name: f.typeParams[index]?.name ?? showTyVarId(index),
      ty: some(p)
    });
  });

  TypeContext.declareTypeParams(newTyCtx, ...inst.typeParams);
  inst.funTy = MonoTy.toPoly(instanceTy);
  inferDecl(inst, newTyCtx, errors);

  if (!ctx.instances.has(f.name.mangled)) {
    ctx.instances.set(f.name.mangled, new Map());
  }

  ctx.instances.get(f.name.mangled)!.set(key, inst);

  return inst;
};
