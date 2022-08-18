import { DataType, genConstructors, match } from "itsamatch";
import { Module, Prog } from "../ast/sweet";
import { Error } from "../errors/errors";
import { lex } from '../parse/lex';
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
    const tokens = Slice.from(lex(source));
    const [decls, errs] = parse(tokens);
    errors.push(...errs.map(Error.Parser));

    for (const decl of decls) {
      await match(decl, {
        Import: async imp => {
          const { imports, path: importPath } = imp;
          const fullPath = fullImportPath(importPath, fs.parentDir(path), fs);
          imp.resolvedPath = fullPath;
          const importedMod = await aux(fullPath);
          const importedMembers = new Set<string>();
          mod.imports.set(fullPath, importedMembers);

          match(imports, {
            all: () => {
              for (const [name, decls] of importedMod.members) {
                decls.forEach(decl => {
                  if (decl.pub) {
                    importedMembers.add(name);
                  }
                });
              }
            },
            names: ({ names }) => {
              names.forEach(name => {
                if (!importedMod.members.has(name)) {
                  errors.push(Error.Resolution(UnknownMember({ modulePath: fullPath, member: name })));
                } else {
                  const members = importedMod.members.get(name)!;
                  members.forEach(member => {
                    if (!member.pub) {
                      errors.push(Error.Resolution(MemberIsNotPublic({ modulePath: fullPath, member: name })));
                    } else {
                      importedMembers.add(name);
                    }
                  });
                }
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
