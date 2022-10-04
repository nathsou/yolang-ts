import fc from "fast-check";
import { Row } from "../infer/structs";
import { Subst } from "../infer/subst";
import { Tuple } from "../infer/tuples";
import { TypeContext } from "../infer/typeContext";
import { MonoTy, TyVar } from "../infer/types";
import { unifyMut, unifyPure } from "../infer/unification";
import { none } from "../utils/maybe";
import { Arb } from './arbitraries/arb';

const testContext = (() => {
  const ctx = TypeContext.make(new Map());
  TypeContext.declareTypeAlias(ctx, 'Yolo', [], MonoTy.int('u32'));
  TypeContext.declareTypeAlias(ctx, 'Hola', [], MonoTy.bool());
  const pairTy = MonoTy.Tuple(Tuple.fromArray([MonoTy.Param('A'), MonoTy.Param('B')]));
  TypeContext.declareTypeAlias(ctx, 'Pair', [{ name: 'A', ty: none }, { name: 'B', ty: none }], pairTy);
  return ctx;
})();

const unify = (a: MonoTy, b: MonoTy, ctx = testContext) => unifyMut(a, b, ctx);

describe('unifyMut', () => {
  it('should unify type variables correctly', () => {
    const tv1 = MonoTy.Var({ kind: 'Unbound', id: 0 });
    const tv2 = MonoTy.Var({ kind: 'Unbound', id: 1 });

    const errs = unify(tv1, tv2);

    expect(errs).toHaveLength(0);
    expect(tv1).toMatchObject(MonoTy.Var({
      kind: 'Link',
      to: tv2,
    }));

    expect(MonoTy.deref(tv1)).toMatchObject(MonoTy.deref(tv2));
  });

  it('should unify a type variable with anything', () => {
    const tyVar = MonoTy.Var({ kind: 'Unbound', id: 0 });
    const tyConst = MonoTy.Const('Yolo');

    const errs = unify(tyVar, tyConst);

    expect(errs).toHaveLength(0);
    expect(tyVar).toMatchObject(MonoTy.Var({
      kind: 'Link',
      to: MonoTy.int('u32'),
    }));

    expect(MonoTy.deref(tyVar)).toMatchObject(MonoTy.int('u32'));
  });

  it('should not take argument order into account', () => {
    const tyVar = MonoTy.Var({ kind: 'Unbound', id: 0 });
    const tyConst = MonoTy.Const('Yolo');

    const errs = unify(tyConst, tyVar);

    expect(errs).toHaveLength(0);
    expect(tyVar).toMatchObject(MonoTy.Var({
      kind: 'Link',
      to: MonoTy.int('u32'),
    }));

    expect(MonoTy.deref(tyVar)).toMatchObject(MonoTy.int('u32'));
  });

  it('should unify type constants correctly', () => {
    const tv1 = MonoTy.Var(TyVar.Unbound(0));
    const tv2 = MonoTy.Var(TyVar.Unbound(1));
    const tc1 = MonoTy.Const('Yolo');
    const tc2 = MonoTy.Const('Hola');
    const pair1 = MonoTy.Const('Pair', tv1, tc1);
    const pair2 = MonoTy.Const('Pair', tc2, tv2);

    const errs = unify(pair1, pair2);

    expect(errs).toHaveLength(0);
    expect(tv1).toMatchObject(MonoTy.Var(TyVar.Link(MonoTy.bool())));
    expect(tv2).toMatchObject(MonoTy.Var(TyVar.Link(MonoTy.int('u32'))));
  });

  it('should fail to resolve undeclared type aliases', () => {
    const emptyCtx = TypeContext.make(new Map());
    const errs = unify(MonoTy.Const('Hola'), MonoTy.Const('u32'), emptyCtx);
    expect(errs.length).toBeGreaterThan(0);
  });

  it('should fail to unify ununifyiable types', () => {
    const tv1 = MonoTy.Var({ kind: 'Unbound', id: 0 });
    const tc1 = MonoTy.Const('Yolo');
    const tc2 = MonoTy.Const('Hola');
    const pair1 = MonoTy.Const('Pair', tv1, tc1);
    const pair2 = MonoTy.Const('Pair', tc2, tv1);

    const errs = unify(pair1, pair2);
    expect(errs).toHaveLength(1);
  });

  it('should unify function types correctly', () => {
    const tv1 = MonoTy.Var({ kind: 'Unbound', id: 0 });
    const tv2 = MonoTy.Var({ kind: 'Unbound', id: 1 });
    const tc1 = MonoTy.Const('Yolo');
    const tc2 = MonoTy.Const('Hola');
    const funTy1 = MonoTy.Fun([tv1, tc1], tc2);
    const funTy2 = MonoTy.Fun([tc2, tv2], tv1);

    const errs = unify(funTy1, funTy2);

    expect(errs).toHaveLength(0);

    expect(tv1).toMatchObject(MonoTy.Var({
      kind: 'Link',
      to: MonoTy.bool(),
    }));

    expect(tv2).toMatchObject(MonoTy.Var({
      kind: 'Link',
      to: MonoTy.int('u32'),
    }));
  });

  it('should unify Struct types correctly', () => {
    const tv1 = MonoTy.fresh();
    const tv2 = MonoTy.fresh();
    const row1 = Row.extend('x', MonoTy.int('u32'), tv1);
    const row2 = Row.extend('y', MonoTy.int('u32'), tv2);
    const rec1 = MonoTy.Struct(row1);
    const rec2 = MonoTy.Struct(row2);

    const errs = unify(rec1, rec2);

    expect(errs).toHaveLength(0);

    expect(tv1).toMatchObject({
      value: {
        kind: 'Link',
        to: {
          variant: 'Struct',
          row: {
            type: 'extend',
            field: 'y',
            ty: MonoTy.int('u32'),
            tail: expect.any(Object),
          }
        },
      },
    });

    expect(tv2).toMatchObject({
      value: {
        kind: 'Link',
        to: {
          variant: 'Struct',
          row: {
            type: 'extend',
            field: 'x',
            ty: MonoTy.int('u32'),
            tail: expect.any(Object),
          }
        },
      },
    });
  });

  it('should unify random types', () => {
    fc.assert(fc.property(Arb.ty(5).chain(ty => fc.tuple(fc.constant(ty), Arb.subst(ty))), ([ty, subst]) => {
      const ty2 = Subst.apply(subst, ty);
      const errs = unify(ty, ty2);
      expect(errs).toHaveLength(0);
      expect(MonoTy.eq(ty, ty2)).toBeTruthy();
    }));
  });
});

describe('unifyPure', () => {
  it('should not mutate type variables', () => {
    const tv1 = MonoTy.Var({ kind: 'Unbound', id: 0 });
    const tv2 = MonoTy.Var({ kind: 'Unbound', id: 1 });

    const ty1 = MonoTy.Tuple(Tuple.fromArray([tv1, tv2]));
    const ty2 = MonoTy.Tuple(Tuple.fromArray([MonoTy.bool(), MonoTy.int('u32')]));

    const subst = unifyPure(ty1, ty2, TypeContext.make(new Map()));

    expect(subst.isOk()).toBeTruthy();
    expect(tv1).toMatchObject(MonoTy.Var({ kind: 'Unbound', id: 0 }));
    expect(tv2).toMatchObject(MonoTy.Var({ kind: 'Unbound', id: 1 }));

    expect(subst.unwrap().get(0)).toMatchObject(MonoTy.bool());
    expect(subst.unwrap().get(1)).toMatchObject(MonoTy.int('u32'));
  });
});
