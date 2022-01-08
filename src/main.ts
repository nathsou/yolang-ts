import { match as matchVariant } from "itsamatch";
import { Decl, Prog } from "./ast/bitter";
import { Context } from './ast/context';
import { Prog as SweetProg } from "./ast/sweet";
import { Error } from './errors/errors';
import { infer } from "./infer/infer";
import { MonoTy, PolyTy, TypeParams } from "./infer/types";
import { createNodeFileSystem } from './resolve/nodefs';
import { resolve } from './resolve/resolve';

// pipeline:
// <string> -> parse -> <sweet> -> desugar & resolve modules -> <bitter> -> infer ->
// monomorphize -> inject reference counting -> emit code

const typeCheck = (sweetProg: SweetProg): [Prog, Error[]] => {
  Context.clear();
  console.log(SweetProg.show(sweetProg) + '\n');
  const errors: Error[] = [];

  const [prog, bitterErrors] = Prog.fromSweet(sweetProg);
  errors.push(...bitterErrors);

  const typingErrors = infer(prog);
  errors.push(...typingErrors);

  return [prog, errors];
};

const run = async (source: string): Promise<Prog> => {
  const nfs = await createNodeFileSystem();
  const [sweetProg, errs1] = await resolve(source, nfs);
  const [prog, errs2] = typeCheck(sweetProg);

  [...errs1, ...errs2].forEach(err => {
    console.log('\x1b[31m%s\x1b[0m', Error.show(err));
  });

  return prog;
};

const showTypes = (decls: Decl[], path: string[]): string[] => {
  const types: string[] = [];
  const pathName = path.length > 0 ? path.join('.') + '.' : '';

  for (const decl of decls) {
    matchVariant(decl, {
      Function: ({ name, funTy }) => {
        types.push(pathName + name.original + ': ' + PolyTy.show(PolyTy.canonicalize(funTy)));
      },
      Module: mod => {
        types.push(...showTypes(mod.decls, [...path, mod.name]));
      },
      TypeAlias: ({ name, typeParams, alias }) => {
        types.push(`type ${pathName}${name}${TypeParams.show(typeParams)} = ${MonoTy.show(alias)}`);
      },
      _: () => { },
    });
  }

  return types;
};

const [, , source] = process.argv;

(async () => {
  if (source) {
    const prog = await run(source);
    console.log(showTypes(prog, []).join('\n\n'));
  } else {
    console.info('Usage: yo <source.yo>');
    process.exit(0);
  }
})();