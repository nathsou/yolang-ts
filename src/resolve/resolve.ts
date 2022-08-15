import { DataType } from "itsamatch";
import { Prog } from "../ast/sweet";
import { Error } from "../errors/errors";
import { Slice } from '../utils/slice';
import { FileSystem } from "./fileSystem";
import { lex } from '../parse/lex';
import { parse } from '../parse/parse';
import { block } from '../utils/misc';

export type ResolutionError = DataType<{
  ModuleNotFound: { name: string },
  TypeNotFound: { path: string[], name: string },
  ModuleAlreadyExists: { name: string, path: string, existingPath: string },
}, 'type'>;

const ext = 'yo';

export const resolve = async (path: string, fs: FileSystem): Promise<[Prog, Error[]]> => {
  const errors: Error[] = [];
  const resolvedPaths = new Set<string>();

  const aux = async (path: string): Promise<Prog> => {
    if (!resolvedPaths.has(path)) {
      resolvedPaths.add(path);
      const source = await fs.readFile(path);
      const tokens = Slice.from(lex(source));
      const [decls, errs] = parse(tokens);
      errors.push(...errs.map(Error.Parser));

      for (const decl of decls) {
        if (decl.variant === 'Import') {
          const { path, imports } = decl;
          const fullPath = block(() => {
            if (path.startsWith('.')) {
              return `${path}.${ext}`;
            }

            return `${fs.stdPath}/${path}.${ext}`;
          });

          decls.unshift(...await aux(fullPath));
        }
      }

      return decls;
    }

    return [];
  };

  return [await aux(path), errors];
};
