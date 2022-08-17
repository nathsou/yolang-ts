import { DataType, genConstructors, match } from "itsamatch";
import { Module, Prog } from "../ast/sweet";
import { Error } from "../errors/errors";
import { lex } from '../parse/lex';
import { parse } from '../parse/parse';
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
export const fullImportPath = (path: string, fs: FileSystem) => fs.resolve(block(() => {
  if (path.startsWith('.')) {
    return `${path}.${ext}`;
  }

  return `${fs.stdPath}/${path}.${ext}`;
}));

export const resolve = async (path: string, fs: FileSystem): Promise<[Prog, Error[]]> => {
  const errors: Error[] = [];
  const modules = new Map<string, Module>();

  const aux = async (path: string): Promise<Module> => {
    const resolvedPath = fs.resolve(path);
    if (modules.has(resolvedPath)) {
      return modules.get(resolvedPath)!;
    }

    const mod: Module = {
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
        Import: async ({ path: importPath, imports }) => {
          const fullPath = fullImportPath(importPath, fs);
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
  const prog: Prog = {
    modules,
    entry: mod,
  };

  return [prog, errors];
};
