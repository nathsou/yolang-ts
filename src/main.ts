import { Expr } from "./ast/sweet";
import { lex } from "./parse/lex";
import { parseExpr } from "./parse/parse";
import { Slice } from "./utils/slice";

const parse = (source: string): Expr => {
  const tokens = lex(source);
  const [expr] = parseExpr(Slice.from(tokens));

  return expr;
};

const res = parse('3 * * + 1 / 4 - (12 * -$)');

console.log(Expr.show(res));