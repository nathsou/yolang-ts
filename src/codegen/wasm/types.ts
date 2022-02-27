import { DataType, match } from "itsamatch";
import { uleb128 } from "./utils";

// https://webassembly.github.io/spec/core/syntax/types.html

export type Byte = number;

export type Vec = Byte[];
export const Vec = {
  encode: (vec: Vec): Byte[] => [...uleb128(vec.length), ...vec],
  encodeMany: (vecs: Vec[]): Byte[] => [...uleb128(vecs.length), ...vecs.flat()],
};

const NUMBER_TYPE_ENCODING = { i32: 0x7f, i64: 0x7e, f32: 0x7d, f64: 0x7c };

export type NumberType = keyof typeof NUMBER_TYPE_ENCODING;
export const NumberType = {
  encode: (ty: NumberType): Byte[] => [NUMBER_TYPE_ENCODING[ty]],
};

const REFERENCE_TYPE_ENCODING = { funcref: 0x70, externref: 0x6f };

export type ReferenceType = keyof typeof REFERENCE_TYPE_ENCODING;
export const ReferenceType = {
  encode: (ty: ReferenceType): Byte[] => [REFERENCE_TYPE_ENCODING[ty]],
};

const VALUE_TYPE_ENCODING = { ...NUMBER_TYPE_ENCODING, ...REFERENCE_TYPE_ENCODING };

export type ValueType = NumberType | ReferenceType | 'none';
export const ValueType = {
  i32: (): ValueType => 'i32',
  none: (): ValueType => 'none',
  encode: (ty: ValueType): Byte[] => ty === 'none' ? [] : [VALUE_TYPE_ENCODING[ty]],
};

export type ResultType = ValueType[];
export const ResultType = {
  encode: (tys: ResultType): Byte[] => Vec.encodeMany(tys.map(ValueType.encode)),
};

export type FuncType = { args: ValueType[], ret: ResultType };
export const FuncType = {
  make: (args: ValueType[], ret: ResultType): FuncType => {
    return {
      args: args.filter(ty => ty !== 'none'),
      ret: ret.filter(ty => ty !== 'none'),
    };
  },
  encode: (ty: FuncType): Byte[] => [
    0x60,
    ...Vec.encodeMany(ty.args.map(ValueType.encode)),
    ...ResultType.encode(ty.ret)
  ],
  show: (ty: FuncType): string => {
    return `(${ty.args.join(', ')}) -> (${ty.ret.join(', ')})`;
  },
};

export type TypeIdx = number;
export const TypeIdx = { encode: uleb128 };

export type LabelIdx = number;
export const LabelIdx = { encode: uleb128 };

export type FuncIdx = number;
export const FuncIdx = { encode: uleb128 };

export type LocalIdx = number;
export const LocalIdx = { encode: uleb128 };

export type GlobalIdx = number;
export const GlobalIdx = { encode: uleb128 };

export type BlockType = DataType<{
  ValueType: { ty: ValueType },
  TypeIdx: { idx: TypeIdx },
  Void: {},
}>;

export const BlockType = {
  ValueType: (ty: ValueType): BlockType => ({ variant: 'ValueType', ty }),
  TypeIdx: (idx: TypeIdx): BlockType => ({ variant: 'TypeIdx', idx }),
  Void: (): BlockType => ({ variant: 'Void' }),
  encode: (ty: BlockType): Byte[] => match(ty, {
    ValueType: ({ ty }) => ValueType.encode(ty),
    TypeIdx: ({ idx }) => TypeIdx.encode(idx),
    Void: () => [0x40],
  }),
  show: (ty: BlockType): string => match(ty, {
    ValueType: ({ ty }) => `${ty}`,
    TypeIdx: ({ idx }) => `type ${idx}`,
    Void: () => 'void',
  }),
};