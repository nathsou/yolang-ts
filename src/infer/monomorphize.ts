import { match, VariantOf } from "itsamatch";
import { Decl, Expr, Prog } from "../ast/bitter";
import { FuncName, NameEnv } from "../ast/name";
import { Error } from "../errors/errors";
import { infer, inferDecl } from "./infer";
import { MonoTy, PolyTy } from "./types";
import { unifyPure } from "./unification";
import { zip } from "../utils/array";
import { Maybe } from "../utils/maybe";
import { id, panic } from "../utils/misc";
import { Either } from "../utils/either";

type MonomorphizedInstance = { typeParams: MonoTy[], func: VariantOf<Decl, 'Function'> };
type GenericFuncs = Map<string, { generic: VariantOf<Decl, 'Function'>, instances: MonomorphizedInstance[] }>;

const monomorphizeModule = (mod: VariantOf<Decl, 'Module'>): VariantOf<Decl, 'Module'> => {
  const genericFuncs: GenericFuncs = new Map();
  const nameEnv = NameEnv.make();
  const topLevelFuncs = new Map<string, VariantOf<Decl, 'Function'>>();

  for (const decl of mod.decls) {
    if (decl.variant === 'Function') {
      topLevelFuncs.set(decl.name.original, decl);
    }
  }

  const mono = { ...mod, decls: [...mod.decls] };

  const monomorphizedFuncs: VariantOf<Decl, 'Function'>[] = [];

  const monomorphizeExpr = (expr: Expr): Expr => match(expr, {
    NamedFuncCall: call => {
      const { args, ty, typeParams } = call;
      const name = call.name.unwrapRight('unresolved func name reference');
      const func = topLevelFuncs.get(name.original)!;

      if (PolyTy.isPolymorphic(func.funTy)) {
        const inst = ((): VariantOf<Decl, 'Function'> => {
          if (genericFuncs.has(name.original)) {
            const instances = genericFuncs.get(name.original)!.instances;

            const inst = instances.find(inst => {
              return zip(inst.typeParams, call.typeParams).every(([a, b]) => {
                return unifyPure(a, b, expr.typeContext!).isOk();
              });
            });

            if (inst) {
              return inst.func;
            }
          }

          const newNameEnv = NameEnv.clone(nameEnv);
          const inst = Decl.rewrite(func, newNameEnv, id, id) as VariantOf<Decl, 'Function'>;
          inst.typeParams = [];
          inst.args = inst.args.map((arg, index) => ({ ...arg, annotation: Maybe.some(args[index].ty) }));
          inst.returnTy = Maybe.some(ty);
          inst.name.renaming = `${func.name.renaming}<${typeParams.map(MonoTy.show).join(', ')}>`;

          const errors: Error[] = [];

          inferDecl(inst, expr.typeContext!, errors);

          if (errors.length > 0) {
            panic(errors.map(err => Error.show(err)).join(', '));
          }

          if (!genericFuncs.has(name.original)) {
            genericFuncs.set(name.original, { generic: func, instances: [] });
          }

          genericFuncs.get(name.original)!.instances.push({ typeParams, func: inst });

          monomorphizedFuncs.push(inst);

          return inst;
        })();

        return { ...call, name: Either.right<string, FuncName>(inst.name) };
      }

      return expr;
    },
    _: () => expr,
  });

  const monomorphizeDecl = (decl: Decl): Decl => {
    return decl;
  };

  const newMod = Decl.rewrite(mono, nameEnv, monomorphizeExpr, monomorphizeDecl) as VariantOf<Decl, 'Module'>;
  newMod.decls.push(...monomorphizedFuncs);

  return newMod;
};

export const monomorphize = (prog: Prog): Prog => {
  const monomorphizedProg: Prog = [];

  for (const decl of prog) {
    match(decl, {
      Module: mod => {
        monomorphizedProg.push(monomorphizeModule(mod));
      },
      _: () => {
        monomorphizedProg.push(decl);
      },
    });
  }

  infer(monomorphizedProg);

  return monomorphizedProg;
};
