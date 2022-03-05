import { DataType, match } from "itsamatch";
import { BinaryOperator, UnaryOperator } from "../../ast/sweet";
import { Maybe, none, some } from "../../utils/maybe";
import { matchString } from "../../utils/misc";
import { Inst } from "./instructions";
import { BlockType, Byte, FuncIdx } from "./types";

export type Expr = DataType<{
  i32: { n: number },
  bool: { b: boolean },
  unit: {},
  unop: { op: UnaryOperator, rhs: Expr },
  binop: { lhs: Expr, op: BinaryOperator, rhs: Expr },
  if: { retTy: BlockType, cond: Expr, then: Expr, else_: Maybe<Expr> },
  block: { retTy: BlockType, exprs: Expr[] },
  return: { expr: Maybe<Expr> },
  unreachable: {},
  nop: {},
  dropValue: { expr: Expr },
  declareVar: { index: number, value: Expr, isGlobal: boolean },
  resolveVar: { index: number, isGlobal: boolean },
  assignment: { index: number, rhs: Expr, isGlobal: boolean },
  call: { func: FuncIdx, args: Expr[] },
  raw: { instructions: Inst[] },
}>;

export const Expr = {
  i32: (n: number): Expr => ({ variant: 'i32', n }),
  bool: (b: boolean): Expr => ({ variant: 'bool', b }),
  unit: (): Expr => ({ variant: 'unit' }),
  unop: (op: UnaryOperator, rhs: Expr): Expr => ({ variant: 'unop', op, rhs }),
  binop: (lhs: Expr, op: BinaryOperator, rhs: Expr): Expr => ({ variant: 'binop', lhs, op, rhs }),
  if: (retTy: BlockType, cond: Expr, then: Expr, else_: Maybe<Expr>): Expr => ({ variant: 'if', retTy, cond, then, else_ }),
  block: (retTy: BlockType, exprs: Expr[]): Expr => ({ variant: 'block', exprs, retTy }),
  return: (expr?: Expr): Expr => ({ variant: 'return', expr: expr ? some(expr) : none }),
  dropValue: (expr: Expr): Expr => ({ variant: 'dropValue', expr }),
  unreachable: (): Expr => ({ variant: 'unreachable' }),
  nop: (): Expr => ({ variant: 'nop' }),
  declareVar: (index: number, value: Expr, { isGlobal } = { isGlobal: false }): Expr => ({ variant: 'declareVar', index, value, isGlobal }),
  resolveVar: (index: number, { isGlobal } = { isGlobal: false }): Expr => ({ variant: 'resolveVar', index, isGlobal }),
  assignment: (index: number, rhs: Expr, { isGlobal } = { isGlobal: false }): Expr => ({ variant: 'assignment', index, rhs, isGlobal }),
  call: (func: FuncIdx, args: Expr[]): Expr => ({ variant: 'call', func, args }),
  raw: (...instructions: Inst[]): Expr => ({ variant: 'raw', instructions }),
  compile: (expr: Expr): Inst[] => match(expr, {
    i32: ({ n }) => [Inst.i32.const(n)],
    bool: ({ b }) => [Inst.i32.const(b ? 1 : 0)],
    unit: () => [],
    unop: ({ op, rhs }) => matchString(op, {
      '-': () => [Inst.i32.const(0), ...Expr.compile(rhs), Inst.i32.sub()],
      '!': () => [...Expr.compile(rhs), Inst.i32.eqz()],
    }),
    binop: ({ lhs, op, rhs }) => {
      if (op === '&&') {
        return Expr.compile(Expr.if(BlockType.ValueType('i32'), lhs, rhs, some(Expr.bool(false))));
      }

      if (op === '||') {
        return Expr.compile(Expr.if(BlockType.ValueType('i32'), lhs, Expr.bool(true), some(rhs)));
      }

      return [...Expr.compile(lhs), ...Expr.compile(rhs), BINOP_MAPPING[op]()];
    },
    if: ({ cond, then, else_, retTy }) => [
      ...Expr.compile(cond),
      Inst.if(retTy),
      ...Expr.compile(then),
      ...else_.mapWithDefault(e => [Inst.else(), ...Expr.compile(e)], []),
      Inst.end()
    ],
    block: ({ exprs }) => exprs.flatMap(Expr.compile),
    return: ({ expr }) => [
      ...expr.mapWithDefault(Expr.compile, []),
      Inst.return(),
    ],
    unreachable: () => [Inst.unreachable()],
    nop: () => [Inst.nop()],
    dropValue: ({ expr }) => [...Expr.compile(expr), Inst.drop()],
    declareVar: ({ index, value, isGlobal }) => [
      ...Expr.compile(value),
      (isGlobal ? Inst.global.set : Inst.local.set)(index)
    ],
    resolveVar: ({ index, isGlobal }) => [
      (isGlobal ? Inst.global.get : Inst.local.get)(index)
    ],
    assignment: ({ index, rhs, isGlobal }) => [
      ...Expr.compile(rhs),
      (isGlobal ? Inst.global.set : Inst.local.set)(index)
    ],
    call: ({ func, args }) => [
      ...args.flatMap(Expr.compile),
      Inst.call(func)
    ],
    raw: ({ instructions }) => instructions,
  }),
  encode: (expr: Expr): Byte[] => Expr.compile(expr).flatMap(Inst.encode),
};

const BINOP_MAPPING: { [op in Exclude<BinaryOperator, '&&' | '||'>]: () => Inst } = {
  '+': Inst.i32.add,
  '-': Inst.i32.sub,
  '*': Inst.i32.mul,
  '/': Inst.i32.div_s,
  '%': Inst.i32.rem_s,
  '==': Inst.i32.eq,
  '!=': Inst.i32.ne,
  '<': Inst.i32.lt_s,
  '<=': Inst.i32.le_s,
  '>': Inst.i32.gt_s,
  '>=': Inst.i32.ge_s,
};