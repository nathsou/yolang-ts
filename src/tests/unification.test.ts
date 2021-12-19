import { Row } from "../infer/records";
import { MonoTy } from "../infer/types";
import { unify } from "../infer/unification";

describe('unification', () => {
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
      to: tyConst,
    }));

    expect(MonoTy.deref(tyVar)).toMatchObject(tyConst);
  });

  it('should not take argument order into account', () => {
    const tyVar = MonoTy.Var({ kind: 'Unbound', id: 0 });
    const tyConst = MonoTy.Const('Yolo');

    const errs = unify(tyConst, tyVar);

    expect(errs).toHaveLength(0);
    expect(tyVar).toMatchObject(MonoTy.Var({
      kind: 'Link',
      to: tyConst,
    }));

    expect(MonoTy.deref(tyVar)).toMatchObject(tyConst);
  });

  it('should unify type constants correctly', () => {
    const tv1 = MonoTy.Var({ kind: 'Unbound', id: 0 });
    const tv2 = MonoTy.Var({ kind: 'Unbound', id: 1 });
    const tc1 = MonoTy.Const('Yolo');
    const tc2 = MonoTy.Const('Hola');
    const pair1 = MonoTy.Const('Pair', tv1, tc1);
    const pair2 = MonoTy.Const('Pair', tc2, tv2);

    const errs = unify(pair1, pair2);

    expect(errs).toHaveLength(0);

    expect(tv1).toMatchObject(MonoTy.Var({
      kind: 'Link',
      to: tc2,
    }));

    expect(tv2).toMatchObject(MonoTy.Var({
      kind: 'Link',
      to: tc1,
    }));
  });

  it.skip('should fail to unify ununifyiable types', () => {
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
      to: tc2,
    }));

    expect(tv2).toMatchObject(MonoTy.Var({
      kind: 'Link',
      to: tc1,
    }));
  });

  it('should unify record types correctly', () => {
    const tv1 = MonoTy.fresh();
    const tv2 = MonoTy.fresh();
    const row1 = Row.extend('x', MonoTy.u32(), tv1);
    const row2 = Row.extend('y', MonoTy.u32(), tv2);
    const rec1 = MonoTy.Record(row1);
    const rec2 = MonoTy.Record(row2);

    const errs = unify(rec1, rec2);

    expect(errs).toHaveLength(0);

    expect(tv1).toMatchObject({
      value: {
        kind: 'Link',
        to: {
          variant: 'Record',
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
        to: {
          variant: 'Record',
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