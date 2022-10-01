import { match } from "itsamatch";
import { Error } from "../errors/errors";
import { FunDecl } from "../infer/env";
import { inferDecl } from "../infer/infer";
import { Subst } from "../infer/subst";
import { TypeContext } from "../infer/typeContext";
import { MonoTy, PolyTy, showTyVarId, TypeParams } from "../infer/types";
import { uniqueBy, zip } from "../utils/array";
import { some } from "../utils/maybe";
import { assert, block, id } from "../utils/misc";
import { Decl, Module, Prog } from "./bitter";
import { NameEnv } from "./name";

export type Mono = {
  Instances: Map<string, Map<string, { params: MonoTy[], fun: FunDecl }>>,
  Context: {
    types: TypeContext,
    instances: Mono['Instances'],
  },
};

const monomorphize = (
  ctx: Mono['Context'],
  f: FunDecl,
  typeParams: MonoTy[],
  errors: Error[]
): FunDecl => {
  const key = TypeParams.hash(typeParams);
  // check if an instance for this overload of f and these type parameters already exists
  if (ctx.instances.has(f.name.mangled) && ctx.instances.get(f.name.mangled)!.has(key)) {
    return ctx.instances.get(f.name.mangled)!.get(key)!.fun;
  }

  const newTyCtx = TypeContext.clone(ctx.types);
  const nameEnv = NameEnv.make();
  const instanceTy = block(() => {
    const instanceTy = PolyTy.instantiatePartially(f.funTy, typeParams);
    assert(!PolyTy.isPolymorphic(instanceTy), 'instantiated generic function is still polymorphic');
    const res = instanceTy[1];
    assert(res.variant === 'Fun');
    return res;
  });

  // substitute the type variables corresponding to type parameters
  // in the generic version of the function with their instances
  const subst = block(() => {
    const subst = Subst.make();
    for (const [{ ty }, inst] of zip(f.typeParams, typeParams)) {
      ty.map(MonoTy.deref).do(ty => {
        if (ty.variant === 'Var' && ty.value.kind === 'Unbound') {
          subst.set(ty.value.id, inst);
        }
      });
    }

    return subst;
  });

  const inst = Decl.rewrite(f, nameEnv, id, ty => MonoTy.substitute(ty, subst)) as FunDecl;
  inst.name.mangled += key;
  inst.returnTy = some(instanceTy.ret);
  inst.args = zip(inst.args, instanceTy.args).map(([a, ty]) => ({
    name: a.name,
    mutable: a.mutable,
    annotation: some(ty)
  }));

  inst.typeParams = [];
  f.typeParams.forEach((_, index) => {
    inst.typeParams.push({
      name: f.typeParams[index]?.name ?? showTyVarId(index),
      ty: some(typeParams[index])
    });
  });

  TypeContext.declareTypeParams(newTyCtx, ...inst.typeParams);
  inst.funTy = MonoTy.toPoly(instanceTy);
  inferDecl(inst, newTyCtx, errors);

  if (!ctx.instances.has(f.name.mangled)) {
    ctx.instances.set(f.name.mangled, new Map());
  }

  ctx.instances.get(f.name.mangled)!.set(key, { params: typeParams, fun: inst });

  return inst;
};

const monomorphizeModule = (mod: Module, instances: Mono['Instances'], errors: Error[]): boolean => {
  const ctx: Mono['Context'] = { types: mod.typeContext, instances };
  let monomorphizedSomething = false;

  for (const decl of mod.decls) {
    if (decl.variant === 'Function' && decl.instances.length > 0) {
      const uniqueInstances = uniqueBy(
        decl.instances.filter(params => params.every(MonoTy.isDetermined)),
        TypeParams.hash
      );

      for (const params of uniqueInstances.values()) {
        monomorphize(ctx, decl, params.map(MonoTy.deref), errors);
        monomorphizedSomething = true;
      }

      decl.instances = [];
    }
  }

  return monomorphizedSomething;
};

const patchGenericFuns = (mod: Module, instances: Mono['Instances'], errors: Error[]): Decl[] => {
  const decls: Decl[] = [];

  for (const decl of mod.decls) {
    match(decl, {
      Function: f => {
        if (instances.has(f.name.mangled)) {
          instances.get(f.name.mangled)!.forEach(({ params }) => {
            const inst = monomorphize({ instances, types: mod.typeContext }, f, params, errors);
            decls.push(inst);
          });
        } else {
          decls.push(f);
        }
      },
      _: () => {
        decls.push(decl);
      },
    });
  }

  return decls;
};

export const Mono = {
  prog: (prog: Prog): [Prog, Mono['Instances'], Error[]] => {
    const errors: Error[] = [];
    const instances: Mono['Instances'] = new Map();
    const monoProg = Prog.shallowClone(prog);

    // monomorphizing a function in a module A might request an instance of a function in B
    while (true) {
      // collect instances
      let monomorphizedSomething = false;
      for (const mod of monoProg.modules.values()) {
        monomorphizedSomething ||= monomorphizeModule(mod, instances, errors);
      }

      if (!monomorphizedSomething) {
        break;
      }
    }

    // replace generic functions with their instances
    for (const mod of monoProg.modules.values()) {
      mod.decls = patchGenericFuns(mod, instances, errors);
    }

    return [monoProg, instances, errors];
  },
};
