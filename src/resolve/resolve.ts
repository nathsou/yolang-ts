import { DataType, genConstructors, match } from "itsamatch";
import { Module, Prog } from "../ast/sweet";
import { Error } from "../errors/errors";
import { lex } from "../parse/lex";
import { parse } from '../parse/parse';
import { groupBy, last } from '../utils/array';
import { block, pushMap } from '../utils/misc';
import { Slice } from '../utils/slice';
import { FileSystem } from "./fileSystem";

export type ResolutionError = DataType<{
  ModuleNotFound: { name: string },
  UnknownMember: { modulePath: string, member: string },
  MemberIsNotPublic: { modulePath: string, member: string },
}>;

const { UnknownMember, MemberIsNotPublic } = genConstructors<ResolutionError>([
  'ModuleNotFound', 'UnknownMember', 'MemberIsNotPublic'
]);

const ext = 'yo';
const fullImportPath = (path: string, dir: string, fs: FileSystem) => fs.resolve(block(() => {
  if (path.startsWith('.')) {
    return `${dir}/${path}.${ext}`;
  }

  return `${fs.stdPath}/${path}.${ext}`;
}));

const fileName = (path: string): string => {
  return last(path.split('/')).replace('.yo', '');
};

const renameModules = (modules: Module[]): void => {
  const names = groupBy(modules, m => fileName(m.path));

  for (const [name, mods] of Object.entries(names)) {
    mods.forEach((mod, index) => {
      const suffix = mods.length === 1 ? '' : `${index}`;
      mod.name = name + suffix;
    });
  }
};

export const resolve = async (path: string, fs: FileSystem): Promise<[Prog, Error[]]> => {
  const errors: Error[] = [];
  const modules = new Map<string, Module>();

  const aux = async (path: string): Promise<Module> => {
    const resolvedPath = fs.resolve(path);
    if (modules.has(resolvedPath)) {
      return modules.get(resolvedPath)!;
    }

    const mod: Module = {
      name: 'tmp',
      path: resolvedPath,
      decls: [],
      members: new Map(),
      imports: new Map(),
    };

    const source = await fs.readFile(path);
    const tokens = lex(source).unwrap();
    const [decls, errs] = parse(Slice.from(tokens));
    errors.push(...errs.map(Error.Parser));

    for (const decl of decls) {
      await match(decl, {
        Import: async imp => {
          const { imports, path: importPath } = imp;
          const fullPath = fullImportPath(importPath, fs.parentDir(path), fs);
          imp.resolvedPath = fullPath;
          const importedMod = await aux(fullPath);

          const resolveImport = (name: string): void => {
            if (importedMod.members.has(name)) {
              const members = importedMod.members.get(name)!;
              for (const member of members) {
                if (!member.pub) {
                  errors.push(Error.Resolution(MemberIsNotPublic({ modulePath: fullPath, member: name })));
                } else {
                  mod.imports.set(name, {
                    sourceMod: importedMod.path,
                    isExport: imp.isExport
                  });
                }
              }
            } else if (importedMod.imports.has(name) && importedMod.imports.get(name)!.isExport) {
              const { sourceMod } = importedMod.imports.get(name)!;
              mod.imports.set(name, { sourceMod, isExport: imp.isExport });
            } else {
              errors.push(Error.Resolution(UnknownMember({ modulePath: fullPath, member: name })));
            }
          };

          match(imports, {
            all: () => {
              for (const [name, decls] of importedMod.members) {
                decls.forEach(decl => {
                  if (decl.pub) {
                    mod.imports.set(name, { sourceMod: importedMod.path, isExport: imp.isExport });
                    resolveImport(name);
                  }
                });
              }

              for (const [name, { sourceMod, isExport }] of importedMod.imports) {
                if (isExport) {
                  mod.imports.set(name, { sourceMod, isExport: imp.isExport });
                }
              }
            },
            names: ({ names }) => {
              names.forEach(name => {
                resolveImport(name);
              });
            },
          });
        },
        Function: f => {
          pushMap(mod.members, f.name, f);
        },
        TypeAlias: t => {
          pushMap(mod.members, t.name, t);
        },
        _: () => { },
      });
    }

    mod.decls = decls;
    modules.set(resolvedPath, mod);

    return mod;
  };

  const mod = await aux(path);

  renameModules([...modules.values()]);

  const prog: Prog = {
    modules,
    entry: mod,
  };

  return [prog, errors];
};
