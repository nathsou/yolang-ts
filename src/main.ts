import { Expr } from "./ast/sweet";
import { formatError } from "./parse/combinators";
import { lex } from "./parse/lex";
import { parseExpr } from "./parse/parse";
import { ok, Result } from "./utils/result";
import { Slice } from "./utils/slice";

const parse = (source: string): Result<Expr, string> => {
  return lex(source).flatMap(tokens => {
    const [expr, errors] = parseExpr(Slice.from(tokens));

    if (errors.length > 0) {
      console.log(`found ${errors.length} error${errors.length > 1 ? 's' : ''}:`);
      for (const error of errors) {
        console.log(formatError(error, tokens));
      }

      console.log('');
    }

    return ok(expr);
  });
};

const res = parse('3 * (7 / ) + 2');

console.log(Expr.show(res.unwrap()));