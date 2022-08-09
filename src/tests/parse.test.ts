import fc from 'fast-check';
import { Context } from '../ast/context';
import { Expr, Pattern } from '../ast/sweet';
import { Row } from '../infer/structs';
import { MonoTy } from '../infer/types';
import { Parser } from '../parse/combinators';
import { lex } from '../parse/lex';
import { binaryExpr, expr, structTy, tuple, unary } from '../parse/parse';
import { Const, Token } from '../parse/token';
import { none } from '../utils/maybe';
import { Slice } from '../utils/slice';
import { Arb } from './arbitraries/arb';

const tokens = (input: string): Slice<Token> => Slice.from(lex(input));

const expectExpr = (parser: Parser<Expr>, input: string, expected: Expr): void => {
  const [res, _, errs] = parser.ref(tokens(input));
  expect(res.isOk()).toBe(true);
  expect(errs).toHaveLength(0);
  expect(res.unwrap()).toEqual(expected);
};

const expectType = (parser: Parser<MonoTy>, input: string, expected: MonoTy): void => {
  const [res, _, errs] = parser.ref(tokens(input));
  expect(res.isOk()).toBe(true);
  expect(errs).toHaveLength(0);
  expect(MonoTy.show(res.unwrap())).toEqual(MonoTy.show(expected));
};

describe('Parser', () => {
  describe('unary operators', () => {
    it('should parse arithmetical negation', () => {
      expectExpr(unary, '-0', Expr.UnaryOp('-', Expr.Const(Const.int(0))));
      expectExpr(unary, '-1', Expr.UnaryOp('-', Expr.Const(Const.int(1))));
      expectExpr(
        unary,
        '-(-1621)',
        Expr.UnaryOp('-', Expr.Parenthesized(Expr.UnaryOp('-', Expr.Const(Const.int(1621)))))
      );
    });

    it('should parse logical negation', () => {
      expectExpr(unary, '!true', Expr.UnaryOp('!', Expr.Const(Const.bool(true))));
      expectExpr(unary, '!false', Expr.UnaryOp('!', Expr.Const(Const.bool(false))));
      expectExpr(
        unary,
        '!(!true)',
        Expr.UnaryOp('!', Expr.Parenthesized(Expr.UnaryOp('!', Expr.Const(Const.bool(true)))))
      );
    });
  });

  describe('binary operators', () => {
    it('should parse addition', () => {
      expectExpr(
        binaryExpr,
        '1 + 2',
        Expr.BinaryOp(Expr.Const(Const.int(1)), '+', Expr.Const(Const.int(2)))
      );
    });
  });

  describe('tuples', () => {
    it('should not parse parenthesized expressions as tuples', () => {
      expectExpr(tuple, '(1)', Expr.Parenthesized(Expr.Const(Const.int(1))));
      expectExpr(tuple, '(())', Expr.Parenthesized(Expr.Const(Const.unit())));
    });

    it('should accept tuples with primary expressions', () => {
      expectExpr(tuple, '(1, 2, 3, 4, 5, 6, 7)', Expr.Tuple([
        Expr.Const(Const.int(1)),
        Expr.Const(Const.int(2)),
        Expr.Const(Const.int(3)),
        Expr.Const(Const.int(4)),
        Expr.Const(Const.int(5)),
        Expr.Const(Const.int(6)),
        Expr.Const(Const.int(7)),
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
        Expr.Const(Const.int(1)),
        Expr.Const(Const.bool(true)),
      ]));

      expectExpr(tuple, '(a, 2, b, c, false, ())', Expr.Tuple([
        Expr.Variable('a'),
        Expr.Const(Const.int(2)),
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
        Expr.Call(Expr.Variable('f'), [], []),
        Expr.Call(Expr.Variable('g'), [], [
          Expr.Const(Const.int(1)),
          Expr.Const(Const.unit()),
        ]),
      ]));
    });

    it('should parse tuples contaning tuples', () => {
      expectExpr(tuple, '(1, (2, 3))', Expr.Tuple([
        Expr.Const(Const.int(1)),
        Expr.Tuple([
          Expr.Const(Const.int(2)),
          Expr.Const(Const.int(3)),
        ]),
      ]));

      expectExpr(tuple, '(1, (2, 3), 4, (5, (6, (7, 8))))', Expr.Tuple([
        Expr.Const(Const.int(1)),
        Expr.Tuple([
          Expr.Const(Const.int(2)),
          Expr.Const(Const.int(3)),
        ]),
        Expr.Const(Const.int(4)),
        Expr.Tuple([
          Expr.Const(Const.int(5)),
          Expr.Tuple([
            Expr.Const(Const.int(6)),
            Expr.Tuple([
              Expr.Const(Const.int(7)),
              Expr.Const(Const.int(8)),
            ]),
          ]),
        ]),
      ]));
    });
  });

  describe('closures', () => {
    it('should parse closures with no parameters', () => {
      expectExpr(expr, '() -> ()', Expr.Closure([], Expr.Const(Const.unit())));
    });

    it('should parse closures with one parameter', () => {
      expectExpr(
        expr,
        'x -> x',
        Expr.Closure(
          [{ pattern: Pattern.Variable('x'), mutable: false, annotation: none }],
          Expr.Variable('x')
        )
      );

      expectExpr(
        expr,
        '(x) -> x',
        Expr.Closure(
          [{ pattern: Pattern.Variable('x'), mutable: false, annotation: none }],
          Expr.Variable('x')
        )
      );
    });

    it('should parse closures with multiple parameters', () => {
      expectExpr(
        expr,
        '(a, b, c) -> (a, b, c)',
        Expr.Closure([
          { pattern: Pattern.Variable('a'), mutable: false, annotation: none },
          { pattern: Pattern.Variable('b'), mutable: false, annotation: none },
          { pattern: Pattern.Variable('c'), mutable: false, annotation: none },
        ],
          Expr.Tuple([
            Expr.Variable('a'),
            Expr.Variable('b'),
            Expr.Variable('c'),
          ])
        )
      );
    });
  });

  describe('struct types', () => {
    it('should parse the empty record type', () => {
      expectType(structTy, '{}', MonoTy.Struct(Row.fromFields([])));
    });

    it('should parse record types without trailing commas', () => {
      expectType(
        structTy,
        '{ yo: u32 }',
        MonoTy.Struct(Row.fromFields([
          ['yo', MonoTy.Const('u32')],
        ]))
      );
    });
  });

  it('should parse randomly generated expressions', () => {
    fc.assert(fc.property(Arb.expr(3), e => {
      Context.clear();
      expectExpr(expr, Expr.show(e), e);
    }));
  });
});