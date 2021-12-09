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
    parsingErrors.forEach(err => {
      console.error(formatError(err, tokens));
    });
    console.log('');
  }

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
  module Logic {
    fn and(a, b) {
      match (a, b) {
        (true, true) => true,
        _ => false,
      }
    }
  
    fn or(a, b) {
      match (a, b) {
        (true, _) => true,
        (_, true) => true,
        _ => false,
      }
    }
  }

  fn factSquared(n) {
    match n * n {
      0 => 1,
      n => n * factSquared(n - 1),
    }
  }

  fn tup((a, b, c)) {
    a + b > 0 || c
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