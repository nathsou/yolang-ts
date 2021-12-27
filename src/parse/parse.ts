import { match as matchVariant } from 'itsamatch';
import { match, select } from 'ts-pattern';
import { Argument, Decl, Expr, Pattern, Prog, Stmt } from '../ast/sweet';
import { RowGeneric } from '../infer/records';
import { TupleGeneric } from '../infer/tuples';
import { MonoTy, ParameterizedTy, TypeParamsContext } from '../infer/types';
import { takeWhile } from '../utils/array';
import { none, some } from '../utils/maybe';
import { compose, ref, snd } from '../utils/misc';
import { error, ok, Result } from '../utils/result';
import { Slice } from '../utils/slice';
import { isLowerCase, isUpperCase } from '../utils/strings';
import { alt, angleBrackets, chainLeft, commas, conditionalError, consumeAll, curlyBrackets, effect, expect, expectOrDefault, initParser, keyword, leftAssoc, lookahead, many, map, mapParserResult, optional, optionalOrDefault, parens, Parser, ParserError, ParserResult, satisfy, satisfyBy, sepBy, seq, symbol, uninitialized, withContext } from './combinators';
import { Const, Token } from './token';

export const expr = uninitialized<Expr>();
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

// TYPES
const monoTy = uninitialized<MonoTy>();

export const parameterizedTy = uninitialized<ParameterizedTy>();
const parenthesizedTy = map(parens(parameterizedTy), ty => ty);
const unitTy = map(seq(symbol('('), symbol(')')), () => ParameterizedTy.Const('()'));
const boolTy = map(ident2('bool'), () => ParameterizedTy.Const('bool'));
const u32Ty = map(ident2('u32'), () => ParameterizedTy.Const('u32'));
const constTy = alt(unitTy, boolTy, u32Ty);
const namedTy = withContext(ctx => map(
  seq(
    upperIdent,
    optionalOrDefault(angleBrackets(optionalOrDefault(commas(parameterizedTy), [])), []),
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
));

const tupleTy = map(
  parens(seq(
    parameterizedTy,
    symbol(','),
    expectOrDefault(commas(parameterizedTy), `Expected type in tuple type after ','`, []),
  )),
  ([h, _, tl]) => ParameterizedTy.Tuple(TupleGeneric.fromArray([h, ...tl]))
);

export const recordTy = map(
  curlyBrackets(optionalOrDefault(commas(seq(
    ident,
    symbol(':'),
    expectOrDefault(parameterizedTy, `Expected type in record type after ':'`, ParameterizedTy.Const('()')),
  )), [])),
  fields => ParameterizedTy.Record(RowGeneric.fromFields(fields.map(([name, _, ty]) => [name, ty])))
);

const allExceptFunTy = alt(
  constTy,
  tupleTy,
  recordTy,
  namedTy,
  parenthesizedTy,
);

const funTy = map(seq(
  alt(
    map(parens(optional(commas(parameterizedTy))), args => args.orDefault([])),
    map(allExceptFunTy, ty => [ty])
  ),
  symbol('->'),
  parameterizedTy,
),
  ([args, _, ret]) => ParameterizedTy.Fun(args, ret)
);

initParser(parameterizedTy, alt(
  funTy,
  allExceptFunTy,
));

initParser(monoTy, map(
  conditionalError(parameterizedTy, ParameterizedTy.isUnparameterized, 'Expected unparameterized type'),
  ty => ParameterizedTy.toPoly(ty, [])[1]
));

const typeParams = angleBrackets(commas(upperIdent));

const typeAnnotation = map(seq(
  symbol(':'),
  expectOrDefault(parameterizedTy, `Expected type after ':'`, ParameterizedTy.Const('()')),
), snd);

const argument = alt<Argument>(
  map(
    seq(
      keyword('mut'),
      expectOrDefault(pattern, `Expected a pattern after 'mut' keyword`, Pattern.Error),
      optional(typeAnnotation),
    ),
    ([_, pattern, annotation]) => ({ pattern, mutable: true, annotation })
  ),
  map(seq(
    pattern,
    optional(typeAnnotation),
  ), ([pattern, annotation]) => ({ pattern, mutable: false, annotation }))
);

const argumentList = map(parens(optional(commas(argument))), args => args.orDefault([]));

// EXPRESSIONS

const integer = satisfyBy<number>(token =>
  match(token)
    .with({
      variant: 'Const',
      value: {
        variant: 'u32',
        value: select()
      }
    },
      n => some(n)
    )
    .otherwise(() => none)
);

const integerConst = map(integer, Const.u32);

const boolConst = satisfyBy<Const>(token =>
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

const unitConst: Parser<Const> = map(
  seq(symbol('('), symbol(')')),
  Const.unit
);

const constVal: Parser<Const> = alt(integerConst, boolConst, unitConst);

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

export const parenthesized = map(parens(expr), Expr.Parenthesized);

export const primary = alt(
  constExpr,
  variable,
  parenthesized,
  block,
  invalid,
  // unexpected
);

const app = leftAssoc(
  primary,
  parens(map(optional(commas(expr)), args => args.orDefault([]))),
  (lhs, rhs) => Expr.Call(lhs, rhs)
);

export const tuple = alt(
  map(
    parens(seq(
      lookahead(tokens => {
        let openParensCount = 1;

        for (const tok of Slice.iter(tokens)) {
          if (tok.variant === 'Symbol' && tok.value === '(') {
            openParensCount++;
          } else if (tok.variant === 'Symbol' && tok.value === ')') {
            openParensCount--;
            if (openParensCount === 0) {
              break;
            }
          }

          if (openParensCount === 1 && tok.variant === 'Symbol' && tok.value === ',') {
            return true;
          }
        }

        // we haven't found a top-level ','
        // so this is a parenthesized expression and not a tuple
        return false;
      }),
      expr,
      symbol(','),
      expectOrDefault(commas(expr), `Expected expression in tuple after ','`, [Expr.Const(Const.unit())]),
    )),
    ([_l, h, _, tl]) => Expr.Tuple([h, ...tl])
  ),
  app
);

const recordField = map(seq(
  expectOrDefault(ident, `Expected field name`, '<?>'),
  expectOrDefault(symbol(':'), `Expected ':' after field name`, Token.Symbol(':')),
  expectOrDefault(expr, `Expected expression after ':'`, Expr.Error),
),
  ([name, _, value]) => ({ name, value })
);

const namedRecord = alt(
  map(seq(
    upperIdent,
    optionalOrDefault(angleBrackets(optionalOrDefault(commas(parameterizedTy), [])), []),
    expectOrDefault(curlyBrackets(optionalOrDefault(commas(recordField), [])), `Expected '{' in named record expression`, []),
  ),
    ([name, params, fields]) => Expr.NamedRecord(name, params, fields)
  ),
  tuple
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
  namedRecord
);

// field access or method call or tuple indexing
const fieldAccess = chainLeft(
  moduleAccess,
  symbol('.'),
  seq(
    expectOrDefault(alt<{ variant: 'ident', name: string } | { variant: 'int', value: number }>(
      map(ident, name => ({ variant: 'ident', name })),
      map(integer, n => ({ variant: 'int', value: n })),
    ), `Expected identifier or integer after '.'`, { variant: 'ident', name: '<?>' }),
    optional(parens(map(optional(commas(expr)), args => args.orDefault([])))),
  ),
  (lhs, _, [field, args]) => args.match({
    Some: args => matchVariant(field, {
      ident: ({ name }) => Expr.MethodCall(lhs, name, args),
      // this will fail in the inferencer as integers are not valid method names
      int: ({ value: n }) => Expr.MethodCall(lhs, `${n}`, args),
    }),
    None: () => matchVariant(field, {
      ident: ({ name }) => Expr.FieldAccess(lhs, name),
      int: ({ value: n }) => Expr.TupleIndexing(lhs, n),
    }),
  })
);

const factor = fieldAccess;

export const unary = alt(
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

export const binary = equality;

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

const letIn = alt(
  map(seq(
    keyword('let'),
    pattern,
    optional(typeAnnotation),
    symbol('='),
    expr,
    keyword('in'),
    expectOrDefault(expr, `Expected expression after 'in' in let expression`, Expr.Error),
  ),
    ([_let, pattern, ann, _eq, val, _in, body]) => Expr.LetIn(pattern, ann, val, body)
  ),
  ifThenElse
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
  letIn
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
      optional(typeAnnotation),
      curlyBrackets(many(matchCase)),
    ),
    ([_, expr, ann, cases]) => Expr.Match(expr, ann, cases)
  ),
  assignment
);

const closure = alt(
  map(
    seq(
      lookahead(tokens => {
        let openParensCount = 0;
        const slice = Slice.clone(tokens);

        do {
          const tok = Slice.head(slice).unwrap();
          if (tok.variant === 'Symbol' && tok.value === '(') {
            openParensCount++;
          } else if (tok.variant === 'Symbol' && tok.value === ')') {
            openParensCount--;
          }

          Slice.stepMut(slice);

        } while (openParensCount > 0 && !Slice.isEmpty(slice));

        // we are after the argument list
        // the next token should be '->'
        return Slice.head(slice).match({
          Some: next => next.variant === 'Symbol' && next.value === '->',
          None: () => false,
        });
      }),
      alt(
        map(parens(
          optional(commas(argument))
        ), args => args.orDefault([])),
        map(pattern, p => [{ pattern: p, mutable: false, annotation: none }])
      ),
      symbol('->'),
      expectOrDefault(expr, `Expected expression after '->'`, Expr.Error),
    ),
    ([_l, args, _, body]) => Expr.Closure(args, body)
  ),
  matchExpr
);

initParser(expr, closure);

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

const letStmt = map(seq(
  alt(keyword('let'), keyword('mut')),
  expectOrDefault(ident, `Expected identifier after 'let' or 'mut' keyword`, '<?>'),
  optional(typeAnnotation),
  expect(symbol('='), `Expected '=' after identifier`),
  expectOrDefault(expr, `Expected expression after '='`, Expr.Error),
),
  ([kw, name, ann, _, expr]) => Stmt.Let(name, expr, Token.eq(kw, Token.Keyword('mut')), ann)
);

const exprStmt = alt(
  map(expr, Stmt.Expr),
  letStmt
);

initParser(
  stmt,
  map(
    // seq(exprStmt, expectOrDefault(symbol(';'), 'Expected semicolon after statement', Token.symbol(';'))),
    seq(exprStmt, optional(symbol(';'))),
    ([stmt]) => stmt
  )
);

// DECLARATIONS

const funcDecl = map(seq(
  keyword('fn'),
  expectOrDefault(ident, `Expected identifier after 'fn' keyword`, '<?>'),
  optionalOrDefault(typeParams, []),
  expectOrDefault(argumentList, 'Expected arguments after function name', []),
  expectOrDefault(block, 'Expected block after function arguments', Expr.Error),
),
  ([_, name, typeParams, args, body]) => Decl.Function(name, typeParams, args, body)
);

const inherentImplDecl = withContext(ctx => map(seq(
  keyword('impl'),
  effect(optionalOrDefault(typeParams, []), params => {
    TypeParamsContext.declare(ctx, ...params);
  }),
  expectOrDefault(parameterizedTy, `Expected type after 'impl' keyword`, ParameterizedTy.Const('()')),
  expectOrDefault(curlyBrackets(many(decl)), `Expected declarations`, []),
),
  ([_, tyParams, ty, decls]) => Decl.Impl(ty, tyParams, decls)
));

const moduleDecl = map(seq(
  keyword('module'),
  expectOrDefault(upperIdent, `Expected an uppercase identifier after 'module' keyword`, '<?>'),
  curlyBrackets(many(decl)),
),
  ([_, name, decls]) => Decl.Module(name, decls)
);

const typeAliasDecl = withContext(ctx => {
  return map(seq(
    keyword('type'),
    expectOrDefault(upperIdent, `Expected identifier after 'type' keyword`, '<?>'),
    effect(optionalOrDefault(typeParams, []), params => {
      TypeParamsContext.declare(ctx, ...params);
    }),
    expectOrDefault(symbol('='), `Expected '=' after type name`, Token.Symbol('=')),
    parameterizedTy,
  ),
    ([_t, name, params, _eq, rhs]) => Decl.TypeAlias(name, params, rhs)
  );
});

initParser(decl, withContext(ctx =>
  ref(tokens => alt(
    funcDecl,
    typeAliasDecl,
    moduleDecl,
    inherentImplDecl,
  ).ref(tokens, TypeParamsContext.clone(ctx)))
));

export const parse = (tokens: Slice<Token>): [Prog, ParserError[]] => {
  const ctx = TypeParamsContext.make();
  const [res, _, errs] = consumeAll(many(decl)).ref(tokens, ctx);

  return res.match({
    Ok: decls => [decls, errs],
    Error: err => [[], [...errs, err]],
  });
};

export const parseRes = compose(parse, Result.wrap);