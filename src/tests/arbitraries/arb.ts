import { Arbitrary } from "fast-check";
import { Expr } from "../../ast/sweet";
import { exprArb } from "./expr.arb";
import { patternArb } from "./pattern.arb";
import { substArb } from "./subst.arb";
import { typeArb } from "./type.arb";

// custom arbitraries
export const Arb = {
  expr: (maxDepth?: number) => exprArb(maxDepth).expr as Arbitrary<Expr>,
  ty: typeArb,
  subst: substArb,
  pattern: patternArb,
};