import { match as matchVariant } from 'itsamatch';
import { match, select } from 'ts-pattern';
import { Decl, Expr, Pattern, Prog, Stmt } from '../ast/sweet';
import { takeWhile } from '../utils/array';
import { none, some } from '../utils/maybe';
import { snd } from '../utils/misc';
import { error, ok } from '../utils/result';
import { Slice } from '../utils/slice';
import { alt, chainLeft, commas, consumeAll, curlyBrackets, expect, expectOrDefault, initParser, keyword, many, map, mapParserResult, optional, parens, Parser, ParserError, ParserResult, satisfy, satisfyBy, sepBy, seq, symbol, uninitialized } from './combinators';
import { Const, Token } from './token';

const expr = uninitialized<Expr>();
const stmt = uninitialized<Stmt>();
const decl = uninitialized<Decl>();
const pattern = uninitialized<Pattern>();

// EXPRESSIONS

const integer = satisfyBy<Const>(token =>
  match(token)
    .with({
      variant: 'Const',
      value: {
        variant: 'u32',
        value: select()
      }
    },
      n => some(Const.u32(n))
    )
    .otherwise(() => none)
);

const bool = satisfyBy<Const>(token =>
  match(token)
    .with({
      variant: 'Const',
      value: {
        variant: 'bool',
        value: select()
      }
    },
      b => some(Const.bool(b))
    )
    .otherwise(() => none)
);

const unit: Parser<Const> = map(
  seq(symbol('('), symbol(')')),
  () => Const.unit()
);

const constVal: Parser<Const> = alt(integer, bool, unit);

const ident = satisfyBy<string>(token => matchVariant(token, {
  Identifier: ({ name }) => some(name),
  _: () => none
}));

const upperIdent = satisfyBy<string>(token => matchVariant(token, {
  Identifier: ({ name }) => name[0].toUpperCase() === name[0] ? some(name) : none,
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

const logicalOp = alt(
  map(symbol('&&'), () => '&&' as const),
  map(symbol('||'), () => '||' as const),
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
  map(symbol('&&='), () => '&&=' as const),
  map(symbol('||='), () => '||=' as const),
);

const variable = map(ident, Expr.Variable);

const block: Parser<Expr> = map(curlyBrackets(many(stmt)), Expr.Block);

const tupleOrParenthesizedExpr = map(
  parens(
    seq(
      expectOrDefault(expr, `Expected expression after '('`, Expr.Error),
      optional(
        seq(
          symbol(','),
          expectOrDefault(commas(expr), `Expected expression in tuple after ','`, []),
        )
      )
    )
  ),
  ([h, tl]) => tl.match({
    Some: ([_, tl]) => tl.length > 0 ? Expr.Tuple([h, ...tl]) : h,
    None: () => h,
  })
);

const constExpr = map(constVal, Expr.Const);

const primary = alt(
  constExpr,
  variable,
  tupleOrParenthesizedExpr,
  block,
  invalid,
  // unexpected
);

// TODO: cleanup and support struct access
// think about how to handle static struct functions (Expr.StaticAccess?)
const attributeAccess = map(
  map(
    seq(
      ident,
      symbol('.'),
      expectOrDefault(sepBy(symbol('.'))(ident), `Expected identifier after '.'`, []),
    ),
    ([name, _, members]) => [name, ...members],
  ),
  path => {
    const modulePath = takeWhile(path, p => p[0].toUpperCase() === p[0]);

    if (modulePath.length === 0) {
      throw new Error(`Expected module path`);
    }

    const members = path.slice(modulePath.length);

    if (members.length !== 1) {
      throw new Error(`Expected single member`);
    }

    return Expr.ModuleAccess(modulePath, members[0]);
  }
);

// e.g Main.Yolo.yo
const moduleAccess = alt(
  attributeAccess,
  primary
);

const app = alt(
  map(
    seq(moduleAccess, parens(optional(commas(expr)))),
    ([lhs, args]) => Expr.Call(lhs, args.orDefault([])),
  ),
  moduleAccess
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

const logical = chainLeft(
  relational,
  logicalOp,
  expect(relational, 'Expected expression after logical operator'),
  (a, op, b) => b.mapWithDefault(
    b => Expr.BinaryOp(a, op, b),
    Expr.Error(`Expected expression after '${op}' operator`)
  )
);

const equality = chainLeft(
  logical,
  equalityOp,
  expect(logical, 'Expected expression after equality operator'),
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

const anyPat = map(symbol('_'), Pattern.Any);
const constPat = map(constVal, Pattern.Const);
const varPat = map(ident, Pattern.Variable);
const tupleOrParenthesizedPat = map(
  parens(
    seq(
      expectOrDefault(pattern, `Expected pattern after '('`, Pattern.Error),
      optional(
        seq(
          symbol(','),
          expectOrDefault(commas(pattern), `Expected pattern in tuple after ','`, []),
        )
      )
    )
  ),
  ([h, tl]) => tl.match({
    Some: ([_, tl]) => tl.length > 0 ? Pattern.Tuple([h, ...tl]) : h,
    None: () => h,
  })
);

initParser(pattern, alt(anyPat, constPat, varPat, tupleOrParenthesizedPat));

const matchCase: Parser<{ pattern: Pattern, body: Expr }> = map(
  seq(
    pattern,
    symbol('=>'),
    expectOrDefault(expr, `Expected an expression after '=>'`, Expr.Block([])),
    expectOrDefault(symbol(','), `Expected ',' after pattern body`, Token.symbol(';')),
  ),
  ([pattern, _, body]) => ({ pattern, body })
);

const matchExpr = alt(
  map(
    seq(
      keyword('match'),
      expectOrDefault(expr, `Expected expression after 'match'`, Expr.Error),
      curlyBrackets(many(matchCase)),
    ),
    ([_, expr, cases]) => Expr.Match(expr, cases)
  ),
  assignment
);

initParser(expr, matchExpr);

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

const moduleDecl = alt(
  map(
    seq(
      keyword('module'),
      expectOrDefault(upperIdent, `Expected an uppercase identifier after 'module' keyword`, '<?>'),
      curlyBrackets(many(decl)),
    ),
    ([_, name, decls]) => Decl.Module(name, decls)
  ),
  funcDecl
);

initParser(decl, moduleDecl);

export const parse = (tokens: Slice<Token>): [Prog, ParserError[]] => {
  const [res, _, errs] = consumeAll(many(moduleDecl)).ref(tokens);

  return res.match({
    Ok: decls => [decls, errs],
    Error: err => [[], [...errs, err]],
  });
};