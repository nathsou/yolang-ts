import { readFileSync } from 'fs';
import { match as matchVariant } from "itsamatch";
import { Decl, Prog } from "./ast/bitter";
import { Prog as SweetProg } from "./ast/sweet";
import { Context } from './ast/context';
import { infer } from "./infer/infer";
import { ParameterizedTy, PolyTy, TypeParams } from "./infer/types";
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

  if (errors.length > 0) {
    console.log(errors.join("\n"));
  }

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
        types.push(`type ${pathName}${name}${TypeParams.show(typeParams)} = ${ParameterizedTy.show(alias)}`);
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