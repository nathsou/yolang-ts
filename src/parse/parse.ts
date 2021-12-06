import { match as matchVariant } from 'itsamatch';
import { match, select } from 'ts-pattern';
import { Decl, Expr, Stmt } from '../ast/sweet';
import { none, some } from '../utils/maybe';
import { snd } from '../utils/misc';
import { error, ok } from '../utils/result';
import { Slice } from '../utils/slice';
import { alt, chainLeft, commas, consumeAll, curlyBrackets, expect, expectOrDefault, initParser, keyword, many, map, mapParserResult, optional, parens, Parser, ParserError, ParserResult, satisfy, satisfyBy, seq, symbol, uninitialized } from './combinators';
import { Const, Token } from './token';

const expr = uninitialized<Expr>();
const stmt = uninitialized<Stmt>();
const decl = uninitialized<Decl>();

// EXPRESSIONS

const integer = satisfyBy<Expr>(token =>
  match(token)
    .with({
      variant: 'Const',
      value: {
        variant: 'u32',
        value: select()
      }
    },
      n => some(Expr.Const(Const.u32(n)))
    )
    .otherwise(() => none)
);

const bool = satisfyBy<Expr>(token =>
  match(token)
    .with({
      variant: 'Const',
      value: {
        variant: 'bool',
        value: select()
      }
    },
      b => some(Expr.Const(Const.bool(b)))
    )
    .otherwise(() => none)
);

const ident = satisfyBy<string>(token => matchVariant(token, {
  Identifier: ({ name }) => some(name),
  _: () => none
}));

const invalid = mapParserResult(
  satisfy(token => token.variant === 'Invalid'),
  ([token, rem, errs]) => {
    return token.match({
      Ok: token => [
        ok(Expr.Error(Token.show(token))),
        rem,
        [...errs, {
          message: Token.show(token),
          pos: rem.start,
        }],
      ],
      Error: err => [error(err), rem, errs] as ParserResult<Expr>,
    });
  }
);

const unexpected = mapParserResult(
  satisfy(() => true),
  ([token, rem, errs]) => {
    return token.match({
      Ok: token => [
        ok(Expr.Error(`Unexpected token: '${Token.show(token)}'`)),
        rem,
        [...errs, { message: `Unexpected token: '${Token.show(token)}'`, pos: rem.start }],
      ],
      Error: err => [
        ok(Expr.Error(err.message)),
        rem,
        errs,
      ],
    });
  }
);

const unaryOp = alt(
  map(symbol('-'), () => '-' as const),
  map(symbol('!'), () => '!' as const),
);

const multiplicativeOp = alt(
  map(symbol('*'), () => '*' as const),
  map(symbol('/'), () => '/' as const),
  map(symbol('%'), () => '%' as const),
);

const additiveOp = alt(
  map(symbol('+'), () => '+' as const),
  map(symbol('-'), () => '-' as const),
);

const relationalOp = alt(
  map(symbol('<'), () => '<' as const),
  map(symbol('>'), () => '>' as const),
  map(symbol('<='), () => '<=' as const),
  map(symbol('>='), () => '>=' as const),
);

const equalityOp = alt(
  map(symbol('=='), () => '==' as const),
  map(symbol('!='), () => '!=' as const),
);

const assignmentOp = alt(
  map(symbol('='), () => '=' as const),
  map(symbol('+='), () => '+=' as const),
  map(symbol('-='), () => '-=' as const),
  map(symbol('*='), () => '*=' as const),
  map(symbol('/='), () => '/=' as const),
  map(symbol('%='), () => '%=' as const),
);

const variable = map(ident, Expr.Variable);

const block: Parser<Expr> = map(curlyBrackets(many(stmt)), Expr.Block);

const primary = alt(
  integer,
  bool,
  variable,
  parens(expectOrDefault(expr, `Expected expression after '('`, Expr.Error)),
  block,
  invalid,
  // unexpected
);

const app = alt(
  map(
    seq(primary, parens(optional(commas(expr)))),
    ([lhs, args]) => Expr.Call(lhs, args.orDefault([])),
  ),
  primary
);

const factor = app;

const unary = alt(
  map(seq(
    unaryOp,
    expectOrDefault(factor, `Expected expression after unary operator`, Expr.Error)
  ), ([op, expr]) => Expr.UnaryOp(op, expr)),
  factor
);

const multiplicative = chainLeft(
  unary,
  multiplicativeOp,
  expect(unary, 'Expected expression after multiplicative operator'),
  (a, op, b) => b.mapWithDefault(
    b => Expr.BinaryOp(a, op, b),
    Expr.Error(`Expected expression after '${op}' operator`)
  )
);

const additive = chainLeft(
  multiplicative,
  additiveOp,
  expect(multiplicative, 'Expected expression after additive operator'),
  (a, op, b) => b.mapWithDefault(
    b => Expr.BinaryOp(a, op, b),
    Expr.Error(`Expected expression after '${op}' operator`)
  )
);

const relational = chainLeft(
  additive,
  relationalOp,
  expect(additive, 'Expected expression after relational operator'),
  (a, op, b) => b.mapWithDefault(
    b => Expr.BinaryOp(a, op, b),
    Expr.Error(`Expected expression after '${op}' operator`)
  )
);

const equality = chainLeft(
  relational,
  equalityOp,
  expect(relational, 'Expected expression after equality operator'),
  (a, op, b) => b.mapWithDefault(
    b => Expr.BinaryOp(a, op, b),
    Expr.Error(`Expected expression after '${op}' operator`)
  )
);

const ifThenElse = alt(
  map(
    seq(
      keyword('if'),
      expectOrDefault(expr, `Expected condition after 'if'`, Expr.Error),
      expectOrDefault(block, `Expected block after condidtion`, Expr.Block([])),
      optional(seq(
        keyword('else'),
        expectOrDefault(block, `Expected block after 'else'`, Expr.Block([])),
      ))
    ),
    ([_, cond, then, else_]) => Expr.IfThenElse(cond, then, else_.map(snd))
  ),
  equality
);

const assignment = alt(
  map(
    seq(
      ifThenElse,
      assignmentOp,
      expectOrDefault(expr, `Expected expression after assignment operator`, Expr.Error),
    ),
    ([lhs, op, rhs]) => {
      if (op === '=') {
        return Expr.Assignment(lhs, rhs);
      } else {
        return Expr.CompoundAssignment(lhs, op, rhs);
      }
    }
  ),
  ifThenElse
);

initParser(expr, assignment);

// STATEMENTS

const exprStmt = map(expr, Stmt.Expr);

const letStmt = alt(
  map(
    seq(
      alt(keyword('let'), keyword('mut')),
      expectOrDefault(ident, `Expected identifier after 'let' or 'mut' keyword`, '<?>'),
      expect(symbol('='), `Expected '=' after identifier`),
      expectOrDefault(expr, `Expected expression after '='`, Expr.Error),
    ),
    ([kw, name, _, expr]) => Stmt.Let(name, expr, Token.eq(kw, Token.keyword('mut')))
  ),
  exprStmt
);

initParser(
  stmt,
  map(
    // seq(letStmt, expectOrDefault(symbol(';'), 'Expected semicolon after statement', Token.symbol(';'))),
    seq(letStmt, optional(symbol(';'))),
    ([stmt]) => stmt
  )
);

const argument = alt(
  map(
    seq(
      keyword('mut'),
      expectOrDefault(ident, `Expected identifier after 'mut' keyword`, '<?>'),
    ),
    ([_, name]) => ({ name, mutable: true })
  ),
  map(ident, name => ({ name, mutable: false }))
);

// DECLARATIONS

const funcDecl = map(
  seq(
    keyword('fn'),
    expectOrDefault(ident, `Expected identifier after 'fn' keyword`, '<?>'),
    expectOrDefault(parens(optional(commas(argument))), 'Expected arguments after function name', none),
    expectOrDefault(block, 'Expected block after function arguments', Expr.Error),
  ),
  ([_, name, args, body]) => Decl.Function(name, args.orDefault([]), body)
);

initParser(decl, funcDecl);

export const parseExpr = (tokens: Slice<Token>): [Expr, ParserError[]] => {
  const [res, _, errs] = consumeAll(expr).ref(tokens);

  return res.match({
    Ok: expr => [expr, errs],
    Error: err => [Expr.Error(err.message), [...errs, err]],
  });
};

export const parseProg = (tokens: Slice<Token>): [Decl[], ParserError[]] => {
  const [res, _, errs] = consumeAll(many(decl)).ref(tokens);

  return res.match({
    Ok: decls => [decls, errs],
    Error: err => [[Decl.Error(err.message)], [...errs, err]],
  });
};