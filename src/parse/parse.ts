import { match, VariantOf } from 'itsamatch';
import { Argument, ArrayInit, Attribute, Decl, Expr, Imports, Pattern, Stmt } from '../ast/sweet';
import { Row } from '../infer/structs';
import { Tuple } from '../infer/tuples';
import { MonoTy, TypeParam } from '../infer/types';
import { deconsLast, last } from '../utils/array';
import { Maybe, none, some } from '../utils/maybe';
import { compose, proj, ref, snd } from '../utils/misc';
import { error, ok, Result } from '../utils/result';
import { Slice } from '../utils/slice';
import { isLowerCase, isUpperCase } from '../utils/strings';
import { alt, chainLeft, commas, consumeAll, curlyBrackets, expect, expectOrDefault, flatMap, initParser, keyword, leftAssoc, lexerContext, lookahead, many, map, mapParserResult, optional, optionalOrDefault, parens, Parser, ParserError, ParserResult, pos, satisfy, satisfyBy, seq, squareBrackets, symbol, uninitialized } from './combinators';
import { Const, Token, TokenWithPos } from './token';

export const expr = uninitialized<Expr>();
const stmt = uninitialized<Stmt>();
const stmt2 = uninitialized<{ stmt: Stmt, discarded: boolean }>();
const decl = uninitialized<Decl>();
const pattern = uninitialized<Pattern>();

// MISC

const ident = satisfyBy<string>(token => match(token, {
  Identifier: ({ name }) => isLowerCase(name[0]) ? some(name) : none,
  _: () => none
}));

const ident2 = (name: string) => satisfy(token => token.variant === 'Identifier' && token.name === name);

const string = <S extends string>(...names: S[]) => map(
  satisfy(token => token.variant === 'Identifier' && names.includes(token.name as S)),
  token => (token as VariantOf<Token, 'Identifier'>).name as S
);

const upperIdent = satisfyBy<string>(token => match(token, {
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
          pos: pos(rem.start),
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
        [...errs, { message: `Unexpected token: '${Token.show(token)}'`, pos: pos(rem.start) }],
      ],
      Error: err => [
        ok(Expr.Error(err.message)),
        rem,
        errs,
      ],
    });
  }
);

const angleBrackets = <T>(p: Parser<T>) => map(
  seq(
    ident2('<'),
    p,
    expect(ident2('>'), `expected closing '>'`),
  ),
  ([_1, r, _2]) => r
);

// TYPES
export const monoTy = uninitialized<MonoTy>();
const parenthesizedTy = map(parens(monoTy), ty => ty);
const voidTy = map(keyword('void'), MonoTy.void);
const boolTy = map(ident2('bool'), MonoTy.bool);
const u32Ty = map(ident2('u32'), MonoTy.u32);
const i32Ty = map(ident2('i32'), MonoTy.i32);
const u64Ty = map(ident2('u64'), MonoTy.u64);
const i64Ty = map(ident2('i64'), MonoTy.i64);
const u8Ty = map(ident2('u8'), MonoTy.u8);
const i8Ty = map(ident2('i8'), MonoTy.i8);
const strTy = map(seq(ident2('str')), MonoTy.str);
const constTy = alt(voidTy, boolTy, u32Ty, i32Ty, u64Ty, i64Ty, u8Ty, i8Ty, strTy);
const namedTy = map(
  seq(
    alt(upperIdent, string('ptr')),
    optionalOrDefault(angleBrackets(commas(monoTy)), []),
  ),
  ([name, args]) => MonoTy.Const(name, ...args),
);

const tupleTy = map(
  parens(seq(
    monoTy,
    symbol(','),
    expectOrDefault(commas(monoTy), `Expected type in tuple type after ','`, []),
  )),
  ([h, _, tl]) => MonoTy.Tuple(Tuple.fromArray([h, ...tl]))
);

export const structTy = map(
  curlyBrackets(commas(seq(
    ident,
    symbol(':'),
    expectOrDefault(monoTy, `Expected type in struct type after ':'`, MonoTy.Const('()')),
  ), true)),
  fields => MonoTy.Struct(Row.fromFields(fields.map(([name, _, ty]) => [name, ty]), false))
);

const allExceptFunTy = alt(
  constTy,
  tupleTy,
  structTy,
  namedTy,
  parenthesizedTy,
);

const funTy = map(seq(
  alt(
    map(parens(optional(commas(monoTy))), args => args.orDefault([])),
    map(allExceptFunTy, ty => [ty])
  ),
  symbol('->'),
  monoTy,
),
  ([args, _, ret]) => MonoTy.Fun(args, ret)
);

const arrayTy = map(
  seq(
    alt(
      funTy,
      allExceptFunTy,
    ),
    optional(seq(symbol('['), symbol(']'))),
  ), ([ty, arr]) => arr.match({
    Some: () => MonoTy.Array(ty),
    None: () => ty,
  }),
);

initParser(monoTy, arrayTy);

const typeParams: Parser<TypeParam[]> = map(
  angleBrackets(commas(upperIdent)),
  ps => ps.map(p => ({ name: p, ty: none }))
);
const typeParamsInst: Parser<MonoTy[]> = angleBrackets(commas(monoTy));

const scopedTypeParams = <T>(p: Parser<T>): Parser<[TypeParam[], T]> =>
  flatMap(optionalOrDefault(typeParams, []), params =>
    ref((tokens: Slice<Token>) =>
      map<T, [TypeParam[], T]>(p, t => [params, t]).ref(tokens)
    )
  );

const typeAnnotation: Parser<MonoTy> = map(seq(
  symbol(':'),
  expectOrDefault(monoTy, `Expected type after ':'`, MonoTy.Const('()')),
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

const integerConst = satisfyBy<Const & { value: number }>(token =>
  token.variant === 'Const' && Const.isInt(token.value) ? some(token.value) : none
);

const boolConst = satisfyBy<Const>(token =>
  token.variant === 'Const' &&
    token.value.variant === 'bool' ?
    some(Const.bool(token.value.value)) :
    none
);

const stringConst = satisfyBy<Const>(token =>
  token.variant === 'Const' && token.value.variant === 'str' ? some(token.value) : none
);

const constVal: Parser<Const> = alt(integerConst, boolConst, stringConst);

const variable = map(ident, Expr.Variable);

const block: Parser<Expr> = map(
  curlyBrackets(many(stmt2)),
  stmts => {
    if (stmts.length > 0 && last(stmts).stmt.variant === 'Expr' && !last(stmts).discarded) {
      const [front, last] = deconsLast(stmts.map(proj('stmt')));
      return Expr.Block(front, some((last as VariantOf<Stmt, 'Expr'>).expr));
    } else {
      return Expr.Block(stmts.map(proj('stmt')), none);
    }
  }
);

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
  seq(optionalOrDefault(typeParamsInst, []), parens(optionalOrDefault(commas(expr), []))),
  (lhs, [tyParams, args]) => Expr.Call(lhs, tyParams, args)
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
      expect(commas(expr), `Expected expression in tuple after ','`),
    )),
    ([_l, h, _, tl]) => Expr.Tuple([h, ...tl])
  ),
  app
);

const array = alt(
  map(squareBrackets(
    alt<ArrayInit>(
      map(seq(expr, symbol(';'), integerConst), ([value, _, count]) => ArrayInit.fill({ value, count: count.value })),
      map(commas(expr, true), elems => ArrayInit.elems({ elems })),
    )
  ), init => Expr.Array(init)),
  tuple
);

const structField = map(seq(
  expectOrDefault(ident, `Expected field name`, '<?>'),
  expectOrDefault(symbol(':'), `Expected ':' after field name`, Token.Symbol(':')),
  expect(expr, `Expected expression after ':'`),
),
  ([name, _, value]) => ({ name, value })
);

const namedStruct = alt(
  map(seq(
    upperIdent,
    optionalOrDefault(angleBrackets(optionalOrDefault(commas(monoTy), [])), []),
    curlyBrackets(commas(structField, true)),
  ),
    ([name, params, fields]) => Expr.Struct(name, params, fields)
  ),
  array
);

// field access or method call or tuple indexing
const fieldAccess = chainLeft(
  namedStruct,
  symbol('.'),
  seq(
    expectOrDefault(alt<{ variant: 'ident', name: string } | { variant: 'int', value: number }>(
      map(ident, name => ({ variant: 'ident', name })),
      map(integerConst, n => ({ variant: 'int', value: n.value })),
    ), `Expected identifier or integer after '.'`, { variant: 'ident', name: '<?>' }),
    optional(parens(map(optional(commas(expr)), args => args.orDefault([])))),
  ),
  (lhs, _, [field, args]) => args.match({
    Some: args => match(field, {
      ident: ({ name }) => Expr.Call(Expr.Variable(name), [], [lhs, ...args]),
      // this will fail in the inferencer as integers are not valid method names
      int: ({ value: n }) => Expr.Call(Expr.Variable(`${n}`), [], [lhs, ...args]),
    }),
    None: () => match(field, {
      ident: ({ name }) => Expr.FieldAccess(lhs, name),
      int: ({ value: n }) => Expr.TupleIndexing(lhs, n),
    }),
  })
);

const indexing = alt(
  map(
    seq(
      fieldAccess,
      squareBrackets(commas(expr, true)),
      optional(seq(
        ident2('='),
        expr,
      )),
    ),
    ([lhs, args, rhs]) => rhs.match({
      None: () => Expr.Call(Expr.Variable('[]'), [], [lhs, ...args]),
      Some: ([_, val]) => Expr.Call(Expr.Variable('[]='), [], [lhs, ...args, val]),
    }),
  ),
  fieldAccess
);

const factor = indexing;

export const unary = alt(
  map(seq(
    string('-', 'not'),
    expectOrDefault(factor, `Expected expression after unary operator`, Expr.Error)
  ), ([op, expr]) => Expr.Call(Expr.Variable(op), [], [expr])),
  factor
);

const multiplicative = chainLeft(
  unary,
  string('*', '/', 'mod'),
  expect(unary, 'Expected expression after multiplicative operator'),
  (a, op, b) => Expr.Call(Expr.Variable(op), [], [a, b])
);

const additive = chainLeft(
  multiplicative,
  string('+', '-'),
  expect(multiplicative, 'Expected expression after additive operator'),
  (a, op, b) => Expr.Call(Expr.Variable(op), [], [a, b])
);

const comparison = chainLeft(
  additive,
  string('<=', '>=', '<', '>'),
  expect(additive, 'Expected expression after relational operator'),
  (a, op, b) => Expr.Call(Expr.Variable(op), [], [a, b])
);

const and = chainLeft(
  comparison,
  string('and', 'nand'),
  expect(comparison, 'Expected expression after logical operator'),
  (a, op, b) => Expr.Call(Expr.Variable(op), [], [a, b]),
);

const or = chainLeft(
  and,
  string('or', 'nor', 'xor', 'xnor'),
  expect(and, 'Expected expression after logical operator'),
  (a, op, b) => Expr.Call(Expr.Variable(op), [], [a, b]),
);

const equality = chainLeft(
  or,
  string('==', '!='),
  expect(or, 'Expected expression after equality operator'),
  (a, op, b) => Expr.Call(Expr.Variable(op), [], [a, b])
);

export const binaryExpr = equality;

const ifThenElse = alt(
  map(
    seq(
      keyword('if'),
      expectOrDefault(expr, `Expected condition after 'if'`, Expr.Error),
      expectOrDefault(block, `Expected block after condidtion`, Expr.Block([])),
      many(
        map(seq(
          keyword('else'), keyword('if'),
          expectOrDefault(expr, `Expected condition after 'else if'`, Expr.Error),
          expectOrDefault(block, `Expected block after condidtion`, Expr.Block([])),
        ),
          ([_e, _i, cond, body]) => ({ cond, body })
        ),
      ),
      optional(seq(
        keyword('else'),
        expectOrDefault(block, `Expected block after 'else'`, Expr.Block([])),
      ))
    ),
    ([_, cond, then, elseifs, else_]) => Expr.IfThenElse(cond, then, elseifs, else_.map(snd))
  ),
  binaryExpr
);

const letIn = alt(
  map(seq(
    keyword('let'),
    pattern,
    optional(typeAnnotation),
    ident2('='),
    expr,
    keyword('in'),
    expectOrDefault(expr, `Expected expression after 'in' in let expression`, Expr.Error),
  ),
    ([_let, pattern, ann, _eq, val, _in, body]) => Expr.LetIn(pattern, ann, val, body)
  ),
  ifThenElse
);

const matchCase: Parser<{ pattern: Pattern, annotation: Maybe<MonoTy>, body: Expr }> = map(
  seq(
    pattern,
    optional(typeAnnotation),
    symbol('=>'),
    expectOrDefault(expr, `Expected an expression after '=>'`, Expr.Block([])),
    expectOrDefault(symbol(','), `Expected ',' after pattern body`, Token.Symbol(',')),
  ),
  ([pattern, annotation, _, body]) => ({ pattern, annotation, body })
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
  letIn
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
  expect(ident2('='), `Expected '=' after identifier`),
  expectOrDefault(expr, `Expected expression after '='`, Expr.Error),
),
  ([kw, name, ann, _, expr]) => Stmt.Let(name, expr, Token.eq(kw, Token.Keyword('mut')), ann)
);

const whileStmt = alt(
  map(
    seq(
      keyword('while'),
      expectOrDefault(expr, `Expected condition after 'while'`, Expr.Error),
      expectOrDefault(curlyBrackets(many(stmt)), `Expected block after condidtion`, []),
    ),
    ([_, cond, stmts]) => Stmt.While(cond, stmts)
  ),
  letStmt
);

const exprStmt = alt(
  map(expr, Stmt.Expr),
  whileStmt
);

const assignmentStmt = alt(
  map(
    seq(
      ifThenElse,
      alt(
        string(
          '=', '+=', '-=', '*=', '/=',
          'mod=', 'and=', 'or=', 'nand=', 'nor=', 'xor=', 'xnor='
        ),
      ),
      expectOrDefault(expr, `Expected expression after assignment operator`, Expr.Error),
    ),
    ([lhs, op, rhs]) => {
      if (op === '=') {
        return Stmt.Assignment(lhs, rhs);
      } else {
        return Stmt.Assignment(lhs, Expr.Call(Expr.Variable(op.slice(0, -1)), [], [lhs, rhs]));
      }
    }
  ),
  exprStmt
);

const returnStmt = alt(
  map(
    seq(keyword('return'), optional(expr)),
    ([_, expr]) => Stmt.Return(expr),
  ),
  assignmentStmt
);

initParser(
  stmt,
  map(
    // seq(exprStmt, expectOrDefault(symbol(';'), 'Expected semicolon after statement', Token.symbol(';'))),
    seq(returnStmt, optional(symbol(';'))),
    ([stmt]) => stmt
  )
);

initParser(
  stmt2,
  map(
    seq(returnStmt, optional(symbol(';'))),
    ([stmt, semicolon]) => ({ stmt, discarded: semicolon.isSome() })
  )
);

// DECLARATIONS

const attribute = map(
  seq(ident, optionalOrDefault(parens(commas(ident)), [])),
  ([name, args]) => Attribute.make(name, args)
);

const attributeList = map(seq(
  symbol('#'),
  alt(
    squareBrackets(commas(attribute)),
    map(attribute, attr => [attr])
  ),
),
  snd
);

const memberVisibility = optionalOrDefault(map(keyword('pub'), () => true), false);

const funcDecl: Parser<VariantOf<Decl, 'Function'>> = map(seq(
  optionalOrDefault(attributeList, []),
  memberVisibility,
  keyword('fun'),
  expectOrDefault(
    alt(
      ident,
      map(
        seq(symbol('['), symbol(']'), optional(ident2('='))),
        ([_1, _2, eq]) => eq.isNone() ? '[]' : '[]='
      )
    ),
    `Expected identifier after 'fun' keyword`,
    '<?>'
  ),
  scopedTypeParams(seq(
    expectOrDefault(argumentList, 'Expected arguments after function name', []),
    optional(seq(
      symbol(':'),
      expect(monoTy, `Expected type after ':'`),
    )),
    optional(block),
  ))
),
  ([attributes, pub, _, name, [typeParams, [args, returnTy, body]]]) => Decl.Function({
    attributes,
    pub,
    name,
    typeParams,
    args,
    returnTy: returnTy.map(snd),
    body,
  })
);

const typeAliasDecl: Parser<Decl> = map(seq(
  memberVisibility,
  keyword('type'),
  expectOrDefault(alt(
    upperIdent,
    string('str'),
    map(seq(symbol('['), symbol(']')), () => '[]')
  ),
    `Expected identifier after 'type' keyword`, '<?>'
  ),
  scopedTypeParams(seq(
    expectOrDefault(ident2('='), `Expected '=' after type name`, Token.Identifier('=')),
    monoTy,
  )),
),
  ([pub, _t, name, [typeParams, [_eq, alias]]]) => Decl.TypeAlias({ pub, name, typeParams, alias })
);

const importPath = map(
  chainLeft(
    map(seq(
      optional(alt(
        map(seq(symbol('.'), ident2('/')), () => './'),
        map(seq(symbol('..'), ident2('/')), () => '../'),
      )),
      ident,
    ), ([l, r]) => [l.orDefault('') + r]),
    ident2('/'),
    ident,
    (lhs, _, rhs) => [...lhs, rhs],
  ),
  path => path.join('/')
);

const importDecl = map(
  seq(
    alt(
      map(keyword('import'), () => false),
      map(keyword('export'), () => true),
    ),
    expect(importPath, `Expected module path after 'import' keyword`),
    optionalOrDefault(
      map(squareBrackets(commas(alt(upperIdent, ident), true)), Imports.names),
      Imports.all(),
    ),
    optional(symbol(';')),
  ),
  ([isExport, path, imports]) => Decl.Import({ isExport, path, resolvedPath: '', imports })
);

initParser(decl, alt(
  funcDecl,
  typeAliasDecl,
  importDecl,
));

export const parse = (tokens: Slice<TokenWithPos>): [Decl[], ParserError[]] => {
  lexerContext.tokens = tokens.elems;
  const [res, _, errs] = consumeAll(many(decl)).ref(tokens);
  lexerContext.tokens = [];

  return res.match({
    Ok: decls => [decls, errs],
    Error: err => [[], [...errs, err]],
  });
};

export const parseRes = compose(parse, Result.wrap);