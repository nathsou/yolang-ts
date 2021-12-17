import { Row } from "../infer/records";
import { MonoTy } from "../infer/types";
import { unify } from "../infer/unification";

describe('unification', () => {
  it('should unify type variables correctly', () => {
    const tv1 = MonoTy.TyVar({ kind: 'Var', id: 0 });
    const tv2 = MonoTy.TyVar({ kind: 'Var', id: 1 });

    const errs = unify(tv1, tv2);

    expect(errs).toHaveLength(0);
    expect(tv1).toMatchObject(MonoTy.TyVar({
      kind: 'Link',
      ref: tv2,
    }));

    expect(MonoTy.deref(tv1)).toMatchObject(MonoTy.deref(tv2));
  });

  it('should unify a type variable with anything', () => {
    const tyVar = MonoTy.TyVar({ kind: 'Var', id: 0 });
    const tyConst = MonoTy.TyConst('Yolo');

    const errs = unify(tyVar, tyConst);

    expect(errs).toHaveLength(0);
    expect(tyVar).toMatchObject(MonoTy.TyVar({
      kind: 'Link',
      ref: tyConst,
    }));

    expect(MonoTy.deref(tyVar)).toMatchObject(tyConst);
  });

  it('should not take argument order into account', () => {
    const tyVar = MonoTy.TyVar({ kind: 'Var', id: 0 });
    const tyConst = MonoTy.TyConst('Yolo');

    const errs = unify(tyConst, tyVar);

    expect(errs).toHaveLength(0);
    expect(tyVar).toMatchObject(MonoTy.TyVar({
      kind: 'Link',
      ref: tyConst,
    }));

    expect(MonoTy.deref(tyVar)).toMatchObject(tyConst);
  });

  it('should unify type constants correctly', () => {
    const tv1 = MonoTy.TyVar({ kind: 'Var', id: 0 });
    const tv2 = MonoTy.TyVar({ kind: 'Var', id: 1 });
    const tc1 = MonoTy.TyConst('Yolo');
    const tc2 = MonoTy.TyConst('Hola');
    const pair1 = MonoTy.TyConst('Pair', tv1, tc1);
    const pair2 = MonoTy.TyConst('Pair', tc2, tv2);

    const errs = unify(pair1, pair2);

    expect(errs).toHaveLength(0);

    expect(tv1).toMatchObject(MonoTy.TyVar({
      kind: 'Link',
      ref: tc2,
    }));

    expect(tv2).toMatchObject(MonoTy.TyVar({
      kind: 'Link',
      ref: tc1,
    }));
  });

  it.skip('should fail to unify ununifyiable types', () => {
    const tv1 = MonoTy.TyVar({ kind: 'Var', id: 0 });
    const tc1 = MonoTy.TyConst('Yolo');
    const tc2 = MonoTy.TyConst('Hola');
    const pair1 = MonoTy.TyConst('Pair', tv1, tc1);
    const pair2 = MonoTy.TyConst('Pair', tc2, tv1);

    const errs = unify(pair1, pair2);

    expect(errs).toHaveLength(1);
  });

  it('should unify function types correctly', () => {
    const tv1 = MonoTy.TyVar({ kind: 'Var', id: 0 });
    const tv2 = MonoTy.TyVar({ kind: 'Var', id: 1 });
    const tc1 = MonoTy.TyConst('Yolo');
    const tc2 = MonoTy.TyConst('Hola');
    const funTy1 = MonoTy.TyFun([tv1, tc1], tc2);
    const funTy2 = MonoTy.TyFun([tc2, tv2], tv1);

    const errs = unify(funTy1, funTy2);

    expect(errs).toHaveLength(0);

    expect(tv1).toMatchObject(MonoTy.TyVar({
      kind: 'Link',
      ref: tc2,
    }));

    expect(tv2).toMatchObject(MonoTy.TyVar({
      kind: 'Link',
      ref: tc1,
    }));
  });

  it.only('should unify record types correctly', () => {
    const tv1 = MonoTy.fresh();
    const tv2 = MonoTy.fresh();
    const row1 = Row.extend('x', MonoTy.u32(), tv1);
    const row2 = Row.extend('y', MonoTy.u32(), tv2);
    const rec1 = MonoTy.TyRecord(row1);
    const rec2 = MonoTy.TyRecord(row2);

    const errs = unify(rec1, rec2);

    expect(errs).toHaveLength(0);

    console.log('row1', MonoTy.show(MonoTy.TyRecord(row1)));
    console.log('row2', MonoTy.show(MonoTy.TyRecord(row2)));

    expect(tv1).toMatchObject({
      value: {
        kind: 'Link',
        ref: {
          variant: 'TyRecord',
          row: {
            type: 'extend',
            field: 'y',
            ty: MonoTy.u32(),
            tail: expect.any(Object),
          }
        },
      },
    });

    expect(tv2).toMatchObject({
      value: {
        kind: 'Link',
        ref: {
          variant: 'TyRecord',
          row: {
            type: 'extend',
            field: 'x',
            ty: MonoTy.u32(),
            tail: expect.any(Object),
          }
        },
      },
    });
  });
});