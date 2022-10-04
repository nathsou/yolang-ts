import fc, { Arbitrary } from 'fast-check';
import { Row } from '../../infer/structs';
import { Tuple } from '../../infer/tuples';
import { MonoTy } from '../../infer/types';
import { uniq } from '../../utils/array';
import { lowerIdent } from './common.arb';

const voidTy = fc.constant(MonoTy.void());
const i32Ty = fc.constant(MonoTy.int('i32'));
const boolTy = fc.constant(MonoTy.bool());
const varTy = fc.integer({ min: 0, max: 10 }).map(n => MonoTy.Var({ kind: 'Unbound', id: n }));

export const ty = (maxDepth = 3) => fc.letrec(tie => ({
  const: fc.frequency(
    { maxDepth },
    { arbitrary: i32Ty, weight: 4 },
    { arbitrary: boolTy, weight: 2 },
    { arbitrary: voidTy, weight: 1 },
  ),
  primary: fc.frequency(
    { maxDepth },
    { arbitrary: tie('const'), weight: 3 },
    { arbitrary: varTy, weight: 1 },
  ),
  tuple: fc.array(tie('ty'), { minLength: 2 }).map(elems => MonoTy.Tuple(Tuple.fromArray(elems as MonoTy[]))),
  struct: fc.array(lowerIdent)
    .map(uniq)
    .chain(fields => fc.tuple(...fields.map(field => fc.tuple(fc.constant(field), tie('ty') as Arbitrary<MonoTy>))))
    .map(fields => MonoTy.Struct(Row.fromFields(fields))),
  function: fc.tuple(fc.array(tie('ty')), tie('ty')).map(([args, ret]) => MonoTy.Fun(args as MonoTy[], ret as MonoTy)),
  ty: fc.frequency(
    { maxDepth },
    { arbitrary: tie('primary'), weight: 3 },
    { arbitrary: tie('tuple'), weight: 1 },
    { arbitrary: tie('struct'), weight: 1 },
    { arbitrary: tie('function'), weight: 1 },
  ),
}));

export const typeArb = (maxDepth?: number) => {
  return ty(maxDepth).ty as Arbitrary<MonoTy>;
};