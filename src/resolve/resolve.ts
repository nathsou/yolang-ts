import { DataType, match as matchVariant, VariantOf } from "itsamatch";
import { Decl, Prog } from "../ast/sweet";
import { Error } from "../errors/errors";
import { lex } from "../parse/lex";
import { parse } from "../parse/parse";
import { last } from "../utils/array";
import { Slice } from "../utils/slice";
import { FileSystem } from "./fileSystem";

export type ResolutionError = DataType<{
  ModuleNotFound: { name: string, path: string },
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
      }

      modules.set(name, {
        path: fullPath,
        name,
        resolved: false
      });
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
        path,
      })]
    ];
  } else {
    const entryMod = modules.get(entryModName)!;
    const mods = await resolveAux(entryMod, fs, modules, errors);

    return [mods, errors];
  }
};

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
  const localTopLevelModules = decls.filter(decl => decl.variant === 'Module') as VariantOf<Decl, 'Module'>[];

  const prog: Prog = [Decl.Module(mod.name, decls)];

  mod.resolved = true;

  const toBeResolved: ModuleRef[] = [];

  Prog.traverse(decls, expr => matchVariant(expr, {
    ModuleAccess: ({ path }) => {
      const modName = moduleName(path[0]);
      if (localTopLevelModules.some(m => m.name === modName)) {
        return;
      }

      if (!modules.has(modName)) {
        errors.push(Error.Resolution({
          type: 'ModuleNotFound',
          name: modName,
          path: path[0],
        }));
      } else {
        const mod = modules.get(modName)!;
        if (!mod.resolved) {
          toBeResolved.push(mod);
        }
      }
    },
    _: () => { },
  }));

  for (const mod of toBeResolved) {
    const subModules = await resolveAux(mod, fs, modules, errors);
    prog.unshift(...subModules);
  }

  return prog;
};