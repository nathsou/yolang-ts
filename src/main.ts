import { Decl, Prog } from "./ast/bitter";
import { infer } from "./infer/infer";
import { formatError } from "./parse/combinators";
import { lex } from "./parse/lex";
import { parse } from "./parse/parse";
import { Slice } from "./utils/slice";
import { match as matchVariant } from "itsamatch";
import { PolyTy } from "./infer/types";

// pipeline:
// <string> -> parse -> <sweet> -> desugar & resolve modules -> <bitter> -> infer ->
// monomorphize -> inject reference counting -> emit code

const run = (source: string): Prog => {
  const tokens = lex(source);
  const [decls, parsingErrors] = parse(Slice.from(tokens));

  if (parsingErrors.length > 0) {
    parsingErrors.forEach(err => console.error(formatError(err, tokens)));
    console.log('');
  }

  const prog = Prog.fromSweet(decls);
  const typingErrors = infer(prog);

  if (typingErrors.length > 0) {
    console.log(typingErrors);
  }

  return prog;
};

const prog = run(`
  module Yo {
    fn yo() {
      1
    }

    fn lo() {
      Lo.hey()
    }

    module Lo {
      fn hey() {
        true
      }
    }
  }
  
  fn run() {
    Main.main()
  }

  module Main {
    fn id(x) { x }

    fn main() {
      id(Yo.yo())
      id(Yo.Lo.hey())
    }
  }
`);

const showFunTypes = (decls: Decl[]): string[] => {
  const types: string[] = [];

  for (const decl of decls) {
    matchVariant(decl, {
      Function: ({ name, funTy }) => {
        types.push(name.original + ': ' + PolyTy.show(funTy));
      },
      Module: mod => {
        const fns = showFunTypes(mod.decls);
        types.push(...fns.map(t => mod.name + '.' + t));
      },
      _: () => { },
    });
  }

  return types;
};

console.log(showFunTypes(prog).join('\n'));