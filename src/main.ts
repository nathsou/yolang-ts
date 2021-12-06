import { Prog } from "./ast/bitter";
import { infer } from "./infer/infer";
import { formatError } from "./parse/combinators";
import { lex } from "./parse/lex";
import { parseProg } from "./parse/parse";
import { Slice } from "./utils/slice";

const parse = (source: string): Prog => {
  const tokens = lex(source);
  const [decls, errs] = parseProg(Slice.from(tokens));

  if (errs.length > 0) {
    errs.forEach(err => console.error(formatError(err, tokens)));
    console.log('');
  }

  return Prog.fromSweet(decls);
};

const decls = parse(`
  fn id(x) { x }

  fn abs(x) {
    if x < 0 { -x } else { x }
  }

  fn fact(n) {
    if n == 0 { 1 } else { n * fact(n - 1) }
  }

  fn odd(n) {
    if n == 0 { false } else { even(n - 1) }
  }

  fn even(n) {
    if n == 0 { true } else { odd(n - 1) }
  }

  fn main() {
    id(abs(-7))
    id(even(fact(11)))
    mut a = 7
    a += 2
  }
`);

const errs = infer(decls);

console.log(errs);