import { Decl, Prog } from "./ast/bitter";
import { Prog as SweetProg } from "./ast/sweet";
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

  console.log(SweetProg.show(decls) + '\n');

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
  fn main() {
    let fst = ((a, b)) -> a;
    let snd = ((a, b)) -> b;
    (fst, snd)
  }
`);

const showFunTypes = (decls: Decl[]): string[] => {
  const types: string[] = [];

  for (const decl of decls) {
    matchVariant(decl, {
      Function: ({ name, funTy }) => {
        types.push(name.original + ': ' + PolyTy.show(PolyTy.canonicalize(funTy)));
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