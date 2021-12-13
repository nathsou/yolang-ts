import { match as matchVariant } from "itsamatch";
import { Decl, Prog } from "./ast/bitter";
import { infer } from "./infer/infer";
import { ParameterizedTy, PolyTy } from "./infer/types";
import { formatError } from "./parse/combinators";
import { lex } from "./parse/lex";
import { parse } from "./parse/parse";
import { Slice } from "./utils/slice";

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
  type Yo<T> = {
    yo: T,
    lo: U
  }

  fn main() {
    (f, g) -> x -> g(f(x))
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
      Struct: ({ name, fields, typeParams }) => {
        types.push(
          'type ' + name + '<' + typeParams.join(', ') + '>' +
          ' = {\n' + fields.map(f => '  ' + f.name + ': ' + ParameterizedTy.show(f.ty)).join(',\n') + '\n}'
        );
      },
      _: () => { },
    });
  }

  return types;
};

console.log(showFunTypes(prog).join('\n'));