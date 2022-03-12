import { BinaryOperator, UnaryOperator } from "../ast/sweet";
import { MonoTy, PolyTy } from "./types";

const unaryOpSignature: Record<UnaryOperator, PolyTy> = {
  '-': MonoTy.toPoly(MonoTy.Fun([MonoTy.u32()], MonoTy.u32())),
  '!': MonoTy.toPoly(MonoTy.Fun([MonoTy.bool()], MonoTy.bool())),
};

const u32OpSig = MonoTy.toPoly(MonoTy.Fun([MonoTy.u32(), MonoTy.u32()], MonoTy.u32()));
const u32BoolOpSig = MonoTy.toPoly(MonoTy.Fun([MonoTy.u32(), MonoTy.u32()], MonoTy.bool()));
const comparisonOpSig = PolyTy.make(
  [0],
  MonoTy.Fun([MonoTy.Var({ kind: 'Unbound', id: 0 }), MonoTy.Var({ kind: 'Unbound', id: 0 })], MonoTy.bool())
);

const logicalOpSig = MonoTy.toPoly(MonoTy.Fun([MonoTy.bool(), MonoTy.bool()], MonoTy.bool()));

const binaryOpSignature: Record<BinaryOperator, PolyTy> = {
  '+': u32OpSig,
  '-': u32OpSig,
  '*': u32OpSig,
  '/': u32OpSig,
  '%': u32OpSig,
  '&': u32OpSig,
  '|': u32OpSig,
  '==': comparisonOpSig,
  '!=': comparisonOpSig,
  '<': u32BoolOpSig,
  '>': u32BoolOpSig,
  '<=': u32BoolOpSig,
  '>=': u32BoolOpSig,
  '&&': logicalOpSig,
  '||': logicalOpSig,
};

export const signatures = {
  unaryOp: unaryOpSignature,
  binaryOp: binaryOpSignature,
  logicalOp: logicalOpSig,
};