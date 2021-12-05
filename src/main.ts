import { Decl } from "./ast/sweet";
import { lex } from "./parse/lex";
import { parseProg } from "./parse/parse";
import { joinWith } from "./utils/array";
import { Slice } from "./utils/slice";

const parse = (source: string): Decl[] => {
  const tokens = lex(source);
  const [decls, errs] = parseProg(Slice.from(tokens));

  if (errs.length > 0) {
    console.log(errs);
  }

  return decls;
};

const decls = parse(`
  fn add(a, b) {
    let sum = a + b
    sum
  }

  fn max(a, b) {
    if a > b { a } else { b }
  }

  fn main() {
    add(3, 7)
  }
`);

console.log(joinWith(decls, Decl.show, '\n\n'));