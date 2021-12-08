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

  module Math {
    fn abs(x) {
      if x < 0 { -x } else { x }
    }

    fn add(a, b) {
      a + b
    }

    fn max(a, b) {
      if a > b { a } else { b }
    }

    fn min(a, b) {
      if a < b { a } else { b }
    }

    fn clamp(x, low, upp) {
      min(max(x, low), upp)
    }

    fn odd(n) {
      if n == 0 { false } else { even(n - 1) }
    }

    fn even(n) {
      if n == 0 { true } else { odd(n - 1) }
    }
  }
  
  fn run() {
    (1, true, 2, 3, true, false)
  }

  module Main {
    fn id(x) { x }

    fn main() {
      id(Yo.yo())
      id(Yo.Lo.hey())
      Math.clamp(218, 0, 127)
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