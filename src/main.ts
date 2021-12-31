import { readFileSync } from 'fs';
import { match as matchVariant } from "itsamatch";
import { Decl, Prog } from "./ast/bitter";
import { Context } from './ast/context';
import { Prog as SweetProg } from "./ast/sweet";
import { infer } from "./infer/infer";
import { MonoTy, PolyTy, TypeParams } from "./infer/types";
import { formatError } from "./parse/combinators";
import { lex } from "./parse/lex";
import { parse } from "./parse/parse";
import { Slice } from "./utils/slice";

// pipeline:
// <string> -> parse -> <sweet> -> desugar & resolve modules -> <bitter> -> infer ->
// monomorphize -> inject reference counting -> emit code

const typeCheck = (source: string): [Prog, string[]] => {
  Context.clear();

  const errors: string[] = [];
  const tokens = lex(source);
  const [decls, parsingErrors] = parse(Slice.from(tokens));
  errors.push(...parsingErrors.map(err => formatError(err, tokens)));

  console.log(SweetProg.show(decls) + '\n');

  const [prog, bitterErrors] = Prog.fromSweet(decls);
  errors.push(...bitterErrors);

  const typingErrors = infer(prog);
  errors.push(...typingErrors);

  return [prog, errors];
};

const run = (source: string): Prog => {
  const [prog, errors] = typeCheck(source);

  errors.forEach(err => {
    console.log('\x1b[31m%s\x1b[0m', err);
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

if (source) {
  const contents = readFileSync(source, 'utf8');
  const prog = run(contents);
  console.log(showTypes(prog, []).join('\n\n'));
} else {
  console.info('Usage: yo <source.yo>');
  process.exit(0);
}