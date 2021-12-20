import fc, { Arbitrary } from 'fast-check';
import { MonoTy } from '../../infer/types';

const unitTy = fc.constant(MonoTy.unit());
const u32Ty = fc.constant(MonoTy.u32());
const boolTy = fc.constant(MonoTy.bool());
const varTy = fc.integer({ min: 0, max: 10 }).map(n => MonoTy.Var({ kind: 'Unbound', id: n }));

export const ty = (maxDepth = 3) => fc.letrec(tie => ({
  const: fc.frequency(
    { maxDepth },
    { arbitrary: u32Ty, weight: 4 },
    { arbitrary: boolTy, weight: 2 },
    { arbitrary: unitTy, weight: 1 },
  ),
  primary: fc.frequency(
    { maxDepth },
    { arbitrary: tie('const'), weight: 3 },
    { arbitrary: varTy, weight: 1 },
  ),
  tuple: fc.array(tie('ty'), { minLength: 2 }).map(elems => MonoTy.tuple(elems as MonoTy[])),
  ty: fc.frequency(
    { maxDepth },
    { arbitrary: tie('primary'), weight: 3 },
    { arbitrary: tie('tuple'), weight: 1 },
  ),
}));

export const typ = (maxDepth?: number) => {
  return ty(maxDepth).ty as Arbitrary<MonoTy>;
};