import { DataType, genConstructors, match } from "itsamatch";
import { Decl, Imports, Module, Prog } from "../ast/sweet";
import { Error } from "../errors/errors";
import { parse } from '../parse/parse';
import { groupBy, last } from '../utils/array';
import { block, pushMap } from '../utils/misc';
import { error, ok, Result } from "../utils/result";
import { FileSystem } from "./fileSystem";

export type ResolutionError = DataType<{
  ModuleNotFound: { name: string },
  UnknownMember: { modulePath: string, member: string },
  MemberIsNotPublic: { modulePath: string, member: string },
  CircularImport: { path: string },
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

export const resolve = async (path: string, fs: FileSystem): Promise<Result<Prog, Error[]>> => {
  const modules = new Map<string, Module>();
  const modulesBeingVisited = new Set<string>();

  const aux = async (path: string, errors: Error[]): Promise<Result<Module, Error[]>> => {
    const resolvedPath = fs.resolve(path);
    if (modules.has(resolvedPath)) {
      return ok(modules.get(resolvedPath)!);
    }

    if (modulesBeingVisited.has(path)) {
      errors.push(Error.Resolution({ variant: 'CircularImport', path }));
      return error(errors);
    }

    modulesBeingVisited.add(path);

    const mod: Module = {
      name: 'tmp',
      path: resolvedPath,
      decls: [],
      members: new Map(),
      imports: new Map(),
      attributes: new Map(),
    };

    const source = await fs.readFile(path);
    const [originalDecls, parsingErrors] = parse(source, path);
    if (parsingErrors.length > 0) {
      errors.push(...parsingErrors);
      return error(errors);
    }

    // implicitly import the foundations
    const decls = block(() => {
      mod.decls = originalDecls;

      // collect module attributes
      originalDecls.forEach(decl => {
        if (decl.variant === 'Attributes') {
          for (const { name, args } of decl.attributes) {
            mod.attributes.set(name, args);
          }
        }
      });

      if (!mod.attributes.has('noImplicitFoundationsImport')) {
        return [
          Decl.Import({
            imports: Imports.all(),
            isExport: false,
            path: 'std/foundations',
            resolvedPath: fs.resolve('std/foundations'),
          }),
          ...originalDecls,
        ];
      } else {
        return originalDecls;
      }
    });

    for (const decl of decls) {
      const prevErrorCount = errors.length;

      await match(decl, {
        Import: async imp => {
          const { imports, path: importPath } = imp;
          const fullPath = fullImportPath(importPath, fs.parentDir(path), fs);
          imp.resolvedPath = fullPath;

          (await aux(fullPath, errors)).match({
            Ok: importedMod => {
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
            Error: () => { },
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

      if (prevErrorCount !== errors.length) {
        break;
      }
    }

    modules.set(resolvedPath, mod);
    modulesBeingVisited.delete(path);

    return Result.wrap([mod, errors]);
  };

  return (await aux(path, [])).map(mod => {
    renameModules([...modules.values()]);

    const prog: Prog = {
      modules,
      entry: mod,
    };

    return prog;
  });
};
