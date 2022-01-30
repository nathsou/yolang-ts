import { DataType, match as matchVariant } from "itsamatch";
import { Decl, Prog } from "../ast/sweet";
import { Error } from "../errors/errors";
import { Row } from "../infer/records";
import { Tuple } from "../infer/tuples";
import { MonoTy } from "../infer/types";
import { lex } from "../parse/lex";
import { parse } from "../parse/parse";
import { filterMap, last } from "../utils/array";
import { Maybe, none, some } from "../utils/maybe";
import { Slice } from "../utils/slice";
import { FileSystem } from "./fileSystem";

export type ResolutionError = DataType<{
  ModuleNotFound: { name: string },
  ModuleAlreadyExists: { name: string, path: string, existingPath: string },
}, 'type'>;

type ModuleRef = {
  path: string,
  name: string,
  resolved: boolean,
};

const moduleName = (path: string) => {
  const name = last(path.split('/')).replace(ext, '').toLowerCase();
  return `${name[0].toUpperCase()}${name.substring(1)}`;
}

const ext = '.yo';

const reachableModules = async (
  dir: string,
  fs: FileSystem,
  errors: Error[],
  modules = new Map<string, ModuleRef>(),
): Promise<Map<string, ModuleRef>> => {
  const files = await fs.readDir(dir);

  for (const file of files) {
    const fullPath = `${dir}/${file.name}`;
    if (file.type === 'file' && file.name.endsWith(ext)) {
      const name = moduleName(file.name);
      if (modules.has(name)) {
        errors.push(Error.Resolution({
          type: 'ModuleAlreadyExists',
          name,
          path: dir,
          existingPath: modules.get(name)!.path,
        }));
      } else {
        modules.set(name, {
          path: fullPath,
          name,
          resolved: false
        });
      }
    } else {
      await reachableModules(fullPath, fs, errors, modules);
    }
  }

  return modules;
};

export const resolve = async (path: string, fs: FileSystem): Promise<[Prog, Error[]]> => {
  const errors: Error[] = [];
  const dir = await fs.directory(path);
  const modules = await reachableModules(dir, fs, errors);
  const entryModName = moduleName(path);

  if (!modules.has(entryModName)) {
    return [
      [],
      [Error.Resolution({
        type: 'ModuleNotFound',
        name: entryModName,
      })]
    ];
  } else {
    const entryMod = modules.get(entryModName)!;
    const mods = await resolveAux(entryMod, fs, modules, errors);

    return [mods, errors];
  }
};

const collectUsedPaths = (ty: MonoTy): string[] => {
  const usedPaths = new Set<string>();
  const registerPath = (path: string[]) => {
    if (path.length > 0) {
      usedPaths.add(moduleName(path[0]));
    }
  };

  const aux = (ty: MonoTy): void => matchVariant(ty, {
    Const: ({ path }) => { registerPath(path); },
    Tuple: ({ tuple }) => Tuple.toArray(tuple).forEach(aux),
    Record: ({ row }) => Row.fields(row).forEach(([_, ty]) => aux(ty)),
    NamedRecord: ({ row }) => Row.fields(row).forEach(([_, ty]) => aux(ty)),
    Fun: ({ args, ret }) => {
      args.forEach(aux);
      aux(ret);
    },
    Error: () => { },
    Param: () => { },
    Var: () => { },
  });

  aux(ty);
  return [...usedPaths];
};

// Collect all references to modules in expressions and types
const resolveAux = async (
  mod: ModuleRef,
  fs: FileSystem,
  modules: Map<string, ModuleRef>,
  errors: Error[]
): Promise<Prog> => {
  const source = await fs.readFile(mod.path);
  const tokens = Slice.from(lex(source));

  const [decls, errs] = parse(tokens);
  errors.push(...errs.map(Error.Parser));
  const localTopLevelModules = new Set(filterMap(decls, d => matchVariant(d, {
    Module: ({ name }) => some(name),
    _: (): Maybe<string> => none,
  })));

  const prog: Prog = [Decl.Module(mod.name, decls)];

  mod.resolved = true;

  const toBeResolved: ModuleRef[] = [];

  const registerModule = (modName: string) => {
    if (localTopLevelModules.has(modName)) {
      return;
    }

    if (!modules.has(modName)) {
      errors.push(Error.Resolution({
        type: 'ModuleNotFound',
        name: modName,
      }));
    } else {
      const mod = modules.get(modName)!;
      if (!mod.resolved) {
        toBeResolved.push(mod);
      }
    }
  };

  Prog.traverse(decls, {
    traverseExpr: expr => matchVariant(expr, {
      ModuleAccess: ({ path }) => {
        registerModule(moduleName(path[0]));
      },
      NamedRecord: ({ path }) => {
        if (path.length > 0) {
          registerModule(moduleName(path[0]));
        }
      },
      Match: ({ annotation }) => {
        annotation.do(ann => {
          collectUsedPaths(ann).forEach(registerModule);
        });
      },
      LetIn: ({ annotation }) => {
        annotation.do(ann => {
          collectUsedPaths(ann).forEach(registerModule);
        });
      },
      Block: ({ statements }) => {
        statements.forEach(s => matchVariant(s, {
          Let: ({ annotation }) => {
            annotation.do(ann => {
              collectUsedPaths(ann).forEach(registerModule);
            });
          },
          _: () => { },
        }));
      },
      _: () => { },
    },
    ),
    traverseDecl: decl => matchVariant(decl, {
      TraitImpl: ({ trait: { path } }) => {
        registerModule(moduleName(path[0]));
      },
      _: () => { },
    }),
  });

  for (const mod of toBeResolved) {
    if (!mod.resolved) {
      const subModules = await resolveAux(mod, fs, modules, errors);
      prog.unshift(...subModules);
    }
  }

  return prog;
};