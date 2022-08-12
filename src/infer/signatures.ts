import { BinaryOperator, UnaryOperator } from "../ast/sweet";
import { MonoTy, PolyTy } from "./types";

const unaryOpSignature: Record<UnaryOperator, PolyTy> = {
  '-': MonoTy.toPoly(MonoTy.Fun([MonoTy.i32()], MonoTy.i32())),
  '!': MonoTy.toPoly(MonoTy.Fun([MonoTy.bool()], MonoTy.bool())),
};

const i32OpSig = MonoTy.toPoly(MonoTy.Fun([MonoTy.i32(), MonoTy.i32()], MonoTy.i32()));
const i32BoolOpSig = MonoTy.toPoly(MonoTy.Fun([MonoTy.i32(), MonoTy.i32()], MonoTy.bool()));
const comparisonOpSig = PolyTy.make(
  [0],
  MonoTy.Fun([MonoTy.Var({ kind: 'Unbound', id: 0 }), MonoTy.Var({ kind: 'Unbound', id: 0 })], MonoTy.bool())
);

const logicalOpSig = MonoTy.toPoly(MonoTy.Fun([MonoTy.bool(), MonoTy.bool()], MonoTy.bool()));

const binaryOpSignature: Record<BinaryOperator, PolyTy> = {
  '+': i32OpSig,
  '-': i32OpSig,
  '*': i32OpSig,
  '/': i32OpSig,
  '%': i32OpSig,
  '&': i32OpSig,
  '|': i32OpSig,
  '==': comparisonOpSig,
  '!=': comparisonOpSig,
  '<': i32BoolOpSig,
  '>': i32BoolOpSig,
  '<=': i32BoolOpSig,
  '>=': i32BoolOpSig,
  '&&': logicalOpSig,
  '||': logicalOpSig,
};

export const signatures = {
  unaryOp: unaryOpSignature,
  binaryOp: binaryOpSignature,
  logicalOp: logicalOpSig,
};