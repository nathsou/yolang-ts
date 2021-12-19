import { match as matchVariant } from "itsamatch";
import { Decl, Prog } from "./ast/bitter";
import { Prog as SweetProg } from "./ast/sweet";
import { infer } from "./infer/infer";
import { ParameterizedTy, PolyTy } from "./infer/types";
import { formatError } from "./parse/combinators";
import { lex } from "./parse/lex";
import { parse } from "./parse/parse";
import { Slice } from "./utils/slice";
import { readFileSync } from 'fs';

// pipeline:
// <string> -> parse -> <sweet> -> desugar & resolve modules -> <bitter> -> infer ->
// monomorphize -> inject reference counting -> emit code

const run = (source: string): Prog => {
  const tokens = lex(source);
  const [decls, parsingErrors] = parse(Slice.from(tokens));

  if (parsingErrors.length > 0) {
    parsingErrors.forEach(err => {
      console.error(formatError(err, tokens));
    });
    console.log('');
  }

  // console.log(SweetProg.show(decls) + '\n');

  const [prog, bitterErrors] = Prog.fromSweet(decls);

  if (bitterErrors.length > 0) {
    console.log(bitterErrors);
  }

  const typingErrors = infer(prog);

  if (typingErrors.length > 0) {
    console.log(typingErrors);
  }

  return prog;
};

const prog = run(`
  module Hoy {
    type Yo<T> = {
      yo: T,
      lo: U
    }

    fn hey(r) {
      r.yo + r.lo
    }
  }

  fn main() {
    (f, g) -> x -> g(f(x))
  }
`);

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
      Struct: ({ name, fields, typeParams }) => {
        types.push(
          'type ' + pathName + name + '<' + typeParams.join(', ') + '>' +
          ' = {\n' + fields.map(f => '  ' + f.name + ': ' + ParameterizedTy.show(f.ty)).join(',\n') + '\n}'
        );
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