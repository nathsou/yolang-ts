import fc from "fast-check";
import { MonoTy } from "../../infer/types";
import { Subst } from "../../infer/subst";
import { typeArb } from "./type.arb";

export const substArb = (ty: MonoTy): fc.Arbitrary<Subst> => {
  return fc.tuple(
    ...[...MonoTy.freeTypeVars(ty)].map(v => fc.tuple(fc.constant(v), fc.boolean()))
  ).chain(vars =>
    fc.tuple(...vars.filter(([_, shouldChange]) => shouldChange).map(([v]) => fc.tuple(fc.constant(v), typeArb())))
      .map(entries => entries.filter(([v, ty]) => !MonoTy.occurs(v, ty)))
  ).map(Subst.from);
};