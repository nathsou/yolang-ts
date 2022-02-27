import { match } from "itsamatch";
import { MonoTy } from "../../infer/types";
import { gen } from "../../utils/array";
import { matchString, panic } from "../../utils/misc";
import { BlockType, Byte, ValueType, Vec } from "./types"

export const uleb128 = (n: number): Byte[] => {
  const bytes: Byte[] = [];

  do {
    let byte = n & 0x7f;
    n >>= 7;
    if (n !== 0) {
      byte = byte | 0x80;
    }
    bytes.push(byte);
  } while (n != 0);

  return bytes;
};

export const sleb128 = (n: number): Byte[] => {
  const bytes: Byte[] = [];
  n |= 0;

  while (true) {
    const byte = n & 0x7f;
    n >>= 7;
    if (
      (n === 0 && (byte & 0x40) === 0) ||
      (n === -1 && (byte & 0x40) !== 0)
    ) {
      bytes.push(byte);
      return bytes;
    }

    bytes.push(byte | 0x80);
  }
};

export const encodeStr = (str: string): Byte[] => Vec.encode(gen(str.length, i => str.charCodeAt(i)));

export const wasmTy = (ty: MonoTy): ValueType => match(ty, {
  Const: c => matchString(c.name, {
    'u32': () => ValueType.i32(),
    'bool': () => ValueType.i32(),
    '()': () => ValueType.none(),
    _: () => {
      return panic(`Unknown type repr for const type: ${c.name}`);
    },
  }),
  Var: v => wasmTy(MonoTy.deref(v)),
  Fun: () => ValueType.i32(), // function index
  _: () => {
    return panic(`wasmTy: type repr not defined yet for ${MonoTy.show(ty)}`);
  },
});

export const blockRetTy = (ty: MonoTy): BlockType => match(ty, {
  Const: c => matchString(c.name, {
    'u32': () => BlockType.ValueType('i32'),
    'bool': () => BlockType.ValueType('i32'),
    '()': () => BlockType.ValueType('none'),
    _: () => {
      panic(`Unknown type repr for const type: ${c.name}`);
      return BlockType.Void();
    },
  }),
  Var: v => blockRetTy(MonoTy.deref(v)),
  Fun: () => BlockType.ValueType('i32'), // function index
  _: () => {
    panic(`blockRetTy: type repr not defined yet for ${MonoTy.show(ty)}`);
    return BlockType.Void();
  },
});