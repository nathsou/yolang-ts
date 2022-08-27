import { match } from "itsamatch";
import { Error } from "../errors/errors";
import { FunDecl } from "../infer/env";
import { inferDecl } from "../infer/infer";
import { TypeContext } from "../infer/typeContext";
import { MonoTy, PolyTy, showTyVarId, TypeParams } from "../infer/types";
import { zip } from "../utils/array";
import { some } from "../utils/maybe";
import { assert, block, id } from "../utils/misc";
import { Decl, Module, Prog } from "./bitter";
import { NameEnv } from "./name";

export type Mono = {
  Instances: Map<string, Map<string, FunDecl>>,
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
    return ctx.instances.get(f.name.mangled)!.get(key)!;
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

const monomorphizeModule = (mod: Module, instances: Mono['Instances'], errors: Error[]): void => {
  const ctx: Mono['Context'] = { types: mod.typeContext, instances };

  for (const decl of mod.decls) {
    if (decl.variant === 'Function' && PolyTy.isPolymorphic(decl.funTy)) {
      for (const params of decl.instances.values()) {
        monomorphize(ctx, decl, params, errors);
      }
    }
  }
};

const patchGenericFuns = (mod: Module, instances: Mono['Instances'], errors: Error[]): Decl[] => {
  const decls: Decl[] = [];

  for (const decl of mod.decls) {
    match(decl, {
      Function: f => {
        if (f.instances.size > 0) {
          f.instances.forEach(params => {
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
  prog: (prog: Prog, errors: Error[]): [Prog, Mono['Instances']] => {
    const instances: Mono['Instances'] = new Map();
    const monoProg = Prog.shallowClone(prog);

    // collect instances
    for (const mod of monoProg.modules.values()) {
      monomorphizeModule(mod, instances, errors);
    }

    // replace generic functions with their instances
    for (const mod of monoProg.modules.values()) {
      mod.decls = patchGenericFuns(mod, instances, errors);
    }

    return [monoProg, instances];
  },
};
