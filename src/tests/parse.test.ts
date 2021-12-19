import { Expr } from '../ast/sweet';
import { Parser } from '../parse/combinators';
import { lex } from '../parse/lex';
import { tuple } from '../parse/parse';
import { Const, Token } from '../parse/token';
import { Slice } from '../utils/slice';

const tokens = (input: string): Slice<Token> => Slice.from(lex(input));

const expectExpr = (parser: Parser<Expr>, input: string, expected: Expr): void => {
  const [res] = parser.ref(tokens(input));
  expect(res.isOk()).toBe(true);
  expect(res.unwrap()).toEqual(expected);
};

describe('Parser', () => {
  describe('tuples', () => {
    it('should not parse parenthesized expressions as tuples', () => {
      const [res1] = tuple.ref(tokens('(1)'));
      expect(res1.isError()).toBe(true);

      const [res2] = tuple.ref(tokens('(())'));
      expect(res2.isError()).toBe(true);
    });

    it('should accept tuples with primary expressions', () => {
      expectExpr(tuple, '(1, 2, 3, 4, 5, 6, 7)', Expr.Tuple([
        Expr.Const(Const.u32(1)),
        Expr.Const(Const.u32(2)),
        Expr.Const(Const.u32(3)),
        Expr.Const(Const.u32(4)),
        Expr.Const(Const.u32(5)),
        Expr.Const(Const.u32(6)),
        Expr.Const(Const.u32(7)),
      ]));

      expectExpr(tuple, '((), ())', Expr.Tuple([
        Expr.Const(Const.unit()),
        Expr.Const(Const.unit()),
      ]));

      expectExpr(tuple, '(true, false, true)', Expr.Tuple([
        Expr.Const(Const.bool(true)),
        Expr.Const(Const.bool(false)),
        Expr.Const(Const.bool(true)),
      ]));

      expectExpr(tuple, '(1, true)', Expr.Tuple([
        Expr.Const(Const.u32(1)),
        Expr.Const(Const.bool(true)),
      ]));

      expectExpr(tuple, '(a, 2, b, c, false, ())', Expr.Tuple([
        Expr.Variable('a'),
        Expr.Const(Const.u32(2)),
        Expr.Variable('b'),
        Expr.Variable('c'),
        Expr.Const(Const.bool(false)),
        Expr.Const(Const.unit()),
      ]));
    });

    it('should parse tuples with compound expressions', () => {
      expectExpr(tuple, '(r.x, r.y + r.z)', Expr.Tuple([
        Expr.FieldAccess(Expr.Variable('r'), 'x'),
        Expr.BinaryOp(
          Expr.FieldAccess(Expr.Variable('r'), 'y'),
          '+',
          Expr.FieldAccess(Expr.Variable('r'), 'z'),
        ),
      ]));

      expectExpr(tuple, '(f(), g(1, ())))', Expr.Tuple([
        Expr.Call(Expr.Variable('f'), []),
        Expr.Call(Expr.Variable('g'), [
          Expr.Const(Const.u32(1)),
          Expr.Const(Const.unit()),
        ]),
      ]));
    });

    it('should parse tuples contaning tuples', () => {
      expectExpr(tuple, '(1, (2, 3))', Expr.Tuple([
        Expr.Const(Const.u32(1)),
        Expr.Tuple([
          Expr.Const(Const.u32(2)),
          Expr.Const(Const.u32(3)),
        ]),
      ]));

      expectExpr(tuple, '(1, (2, 3), 4, (5, (6, (7, 8))))', Expr.Tuple([
        Expr.Const(Const.u32(1)),
        Expr.Tuple([
          Expr.Const(Const.u32(2)),
          Expr.Const(Const.u32(3)),
        ]),
        Expr.Const(Const.u32(4)),
        Expr.Tuple([
          Expr.Const(Const.u32(5)),
          Expr.Tuple([
            Expr.Const(Const.u32(6)),
            Expr.Tuple([
              Expr.Const(Const.u32(7)),
              Expr.Const(Const.u32(8)),
            ]),
          ]),
        ]),
      ]));
    });
  });
});