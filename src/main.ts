import { lex } from "./parse/lex";
import { parseExpr } from "./parse/parse";
import { Slice } from "./utils/slice";

const parse = (source: string) => {
  return lex(source).flatMap(tokens => parseExpr(Slice.from(tokens)));
};

const res = parse('3 * 7 + -1');

console.log(res.unwrap());