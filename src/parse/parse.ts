import { match as matchVariant } from 'itsamatch';
import { match, select } from 'ts-pattern';
import { Argument, Decl, Expr, Pattern, Prog, Stmt } from '../ast/sweet';
import { MonoTy, ParameterizedTy, TypeParamsContext } from '../infer/types';
import { takeWhile } from '../utils/array';
import { none, some } from '../utils/maybe';
import { compose, ref, snd } from '../utils/misc';
import { error, ok, Result } from '../utils/result';
import { Slice } from '../utils/slice';
import { isLowerCase, isUpperCase } from '../utils/strings';
import { alt, angleBrackets, chainLeft, commas, conditionalError, consumeAll, curlyBrackets, expect, expectOrDefault, initParser, keyword, lazy, leftAssoc, many, map, mapParserResult, optional, optionalOrDefault, parens, Parser, ParserError, ParserResult, satisfy, satisfyBy, sepBy, seq, symbol, uninitialized } from './combinators';
import { Const, Token } from './token';

const expr = uninitialized<Expr>();
const stmt = uninitialized<Stmt>();
const decl = uninitialized<Decl>();
const pattern = uninitialized<Pattern>();

// MISC

const ident = satisfyBy<string>(token => matchVariant(token, {
  Identifier: ({ name }) => isLowerCase(name[0]) ? some(name) : none,
  _: () => none
}));

const ident2 = (name: string) => satisfy(token => token.variant === 'Identifier' && token.name === name);

const upperIdent = satisfyBy<string>(token => matchVariant(token, {
  Identifier: ({ name }) => isUpperCase(name[0]) ? some(name) : none,
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

const argument = alt<Argument>(
  map(
    seq(
      keyword('mut'),
      expectOrDefault(pattern, `Expected a pattern after 'mut' keyword`, Pattern.Error),
    ),
    ([_, pattern]) => ({ pattern, mutable: true })
  ),
  map(pattern, pattern => ({ pattern, mutable: false }))
);

const argumentList = map(parens(optional(commas(argument))), args => args.orDefault([]));

// TYPES
const monoTy = ref<(ctx: TypeParamsContext) => Parser<MonoTy>>(uninitialized);

const parameterizedTy = (ctx: TypeParamsContext): Parser<ParameterizedTy> => {
  const parameterizedMonoTy = uninitialized<ParameterizedTy>();
  const parenthesizedTy = map(parens(parameterizedMonoTy), ty => ty);
  const unitTy = map(seq(symbol('('), symbol(')')), () => ParameterizedTy.Const('()'));
  const boolTy = map(ident2('bool'), () => ParameterizedTy.Const('bool'));
  const u32Ty = map(ident2('u32'), () => ParameterizedTy.Const('u32'));
  const constTy = alt(unitTy, boolTy, u32Ty);
  const namedTy = map(
    seq(
      upperIdent,
      optionalOrDefault(angleBrackets(many(parameterizedMonoTy)), []),
    ),
    ([name, args]) => {
      if (args.length === 0) {
        if (TypeParamsContext.has(ctx, name)) {
          return ParameterizedTy.Param(name);
        } else {
          return ParameterizedTy.Const(name);
        }
      } else {
        return ParameterizedTy.Const(name, ...args);
      }
    }
  );

  const tupleTy = map(
    parens(seq(
      parameterizedMonoTy,
      symbol(','),
      expectOrDefault(commas(parameterizedMonoTy), `Expected type in tuple type after ','`, []),
    )),
    ([h, _, tl]) => ParameterizedTy.Const('tuple', h, ...tl)
  );

  const allExceptFunTy = alt(
    constTy,
    tupleTy,
    namedTy,
    parenthesizedTy,
  );

  const funTy = map(seq(
    alt(
      parens(commas(parameterizedMonoTy)),
      map(allExceptFunTy, ty => [ty])
    ),
    symbol('->'),
    parameterizedMonoTy,
  ),
    ([args, _, ret]) => ParameterizedTy.Fun(args, ret)
  );

  parameterizedMonoTy.ref = alt(
    allExceptFunTy,
    funTy,
  ).ref;

  return parameterizedMonoTy;
};

monoTy.ref = (ctx: TypeParamsContext) => {
  return map(
    conditionalError(parameterizedTy(ctx), ParameterizedTy.isUnparameterized, 'Expected unparameterized type'),
    ty => ParameterizedTy.toPoly(ty, [])[1]
  );
};

const typeParams = angleBrackets(commas(upperIdent));

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
  Const.unit
);

const constVal: Parser<Const> = alt(integer, bool, unit);

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

const constExpr = map(constVal, Expr.Const);

const primary = alt(
  constExpr,
  variable,
  parens(expr),
  block,
  invalid,
  // unexpected
);

const app = leftAssoc(
  primary,
  parens(map(optional(commas(expr)), args => args.orDefault([]))),
  (lhs, rhs) => Expr.Call(lhs, rhs)
);

// e.g Main.Yolo.yo
const moduleAccess = alt(
  map(
    map(
      seq(
        upperIdent,
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
  ),
  app
);

const fieldAccess = chainLeft(
  moduleAccess,
  symbol('.'),
  expectOrDefault(ident, `Expected identifier after '.'`, '<?>'),
  (lhs, _, field) => Expr.FieldAccess(lhs, field)
);

const factor = fieldAccess;

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

const matchCase: Parser<{ pattern: Pattern, body: Expr }> = map(
  seq(
    pattern,
    symbol('=>'),
    expectOrDefault(expr, `Expected an expression after '=>'`, Expr.Block([])),
    expectOrDefault(symbol(','), `Expected ',' after pattern body`, Token.Symbol(',')),
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

const closure = alt(
  map(
    seq(
      alt(argumentList, map(pattern, p => [{ pattern: p, mutable: false }])),
      symbol('->'),
      expectOrDefault(expr, `Expected expression after '->'`, Expr.Error),
    ),
    ([args, _, body]) => Expr.Closure(args, body)
  ),
  matchExpr
);

export const tuple = alt(
  map(
    parens(seq(
      expr,
      symbol(','),
      expectOrDefault(commas(expr), `Expected expression in tuple after ','`, [Expr.Const(Const.unit())]),
    )),
    ([h, _, tl]) => Expr.Tuple([h, ...tl])
  ),
  closure
);

initParser(expr, tuple);

// PATTERNS

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
    ([kw, name, _, expr]) => Stmt.Let(name, expr, Token.eq(kw, Token.Keyword('mut')))
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

// DECLARATIONS

const funcDecl = map(
  seq(
    keyword('fn'),
    expectOrDefault(ident, `Expected identifier after 'fn' keyword`, '<?>'),
    expectOrDefault(argumentList, 'Expected arguments after function name', []),
    expectOrDefault(block, 'Expected block after function arguments', Expr.Error),
  ),
  ([_, name, args, body]) => Decl.Function(name, args, body)
);

const moduleDecl = lazy(() => map(
  seq(
    keyword('module'),
    expectOrDefault(upperIdent, `Expected an uppercase identifier after 'module' keyword`, '<?>'),
    curlyBrackets(many(decl)),
  ),
  ([_, name, decls]) => Decl.Module(name, decls)
));

const structField = (ctx: TypeParamsContext) => {
  return map(
    seq(
      expectOrDefault(ident, `Expected field name`, '<?>'),
      expectOrDefault(symbol(':'), `Expected ':' after field name`, Token.Symbol(':')),
      expectOrDefault(lazy(() => parameterizedTy(ctx)), `Expected type after ':'`, ParameterizedTy.Const('()')),
    ),
    ([name, _, ty]) => ({ name, ty })
  );
};

const typeDecl = lazy(() => {
  const ctx = TypeParamsContext.make();

  return map(
    seq(
      keyword('type'),
      expectOrDefault(upperIdent, `Expected identifier after 'type' keyword`, '<?>'),
      map(optionalOrDefault(typeParams, []), params => {
        for (const p of params) {
          TypeParamsContext.declare(ctx, p);
        }

        return params;
      }),
      expectOrDefault(symbol('='), `Expected '=' after type name`, Token.Symbol('=')),
      alt(
        map(curlyBrackets(commas(lazy(() => structField(ctx)))), fields => ({ type: 'struct' as const, fields }))
      ),
    ),
    ([_t, name, params, _eq, rhs]) => {
      switch (rhs.type) {
        case 'struct':
          return Decl.NamedRecord(name, params, rhs.fields);
      }
    }
  );
});

initParser(decl, alt(funcDecl, typeDecl, moduleDecl));

export const parse = (tokens: Slice<Token>): [Prog, ParserError[]] => {
  const [res, _, errs] = consumeAll(many(decl)).ref(tokens);

  return res.match({
    Ok: decls => [decls, errs],
    Error: err => [[], [...errs, err]],
  });
};

export const parseRes = compose(parse, Result.wrap);