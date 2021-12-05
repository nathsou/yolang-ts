import { Decl as BitterDecl } from "./ast/bitter";
import { Decl } from "./ast/sweet";
import { formatError } from "./parse/combinators";
import { lex } from "./parse/lex";
import { parseProg } from "./parse/parse";
import { Slice } from "./utils/slice";

const parse = (source: string): Decl[] => {
  const tokens = lex(source);
  const [decls, errs] = parseProg(Slice.from(tokens));

  if (errs.length > 0) {
    errs.forEach(err => console.error(formatError(err, tokens)));
    console.log('');
  }

  return decls;
};

const decls = parse(`
  fn main() {
    mut n = 3
    n += 7
  }
`);

const nameEnv = {};
const bitter = decls.map(d => BitterDecl.fromSweet(d, nameEnv));

// console.log(joinWith(decls, Decl.show, '\n\n'));

console.log(JSON.stringify(bitter, null, 2));