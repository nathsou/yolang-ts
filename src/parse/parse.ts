import { match, VariantOf } from 'itsamatch';
import { Argument, ArrayInit, Attribute, Decl, Expr, Imports, Pattern, Stmt } from '../ast/sweet';
import { Error } from '../errors/errors';
import { Row } from '../infer/structs';
import { Tuple } from '../infer/tuples';
import { MonoTy, TypeParam } from '../infer/types';
import { deconsLast, last } from '../utils/array';
import { Maybe, none, some } from '../utils/maybe';
import { proj, ref, snd } from '../utils/misc';
import { error, ok } from '../utils/result';
import { Slice } from '../utils/slice';
import { isLowerCase, isUpperCase } from '../utils/strings';
import { alt, chainLeft, commas, consumeAll, curlyBrackets, expect, expectOrDefault, flatMap, initParser, keyword, leftAssoc, lexerContext, lookahead, many, map, mapParserResult, optional, optionalOrDefault, parens, Parser, ParserResult, pos, satisfy, satisfyBy, seq, squareBrackets, symbol, uninitialized } from './combinators';
import { lex } from './lex';
import { Const, Token } from './token';

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
        ok(Expr.Error(Token.show(token), token.pos)),
        rem,
        [...errs, Error.Parser({
          message: Token.show(token),
        }, pos(rem.start))],
      ],
      Error: err => [error(err), rem, errs] as ParserResult<Expr>,
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
const i8Ty = map(ident2('i8'), () => MonoTy.int('i8'));
const u8Ty = map(ident2('u8'), () => MonoTy.int('u8'));
const i16Ty = map(ident2('i16'), () => MonoTy.int('i16'));
const u16Ty = map(ident2('u16'), () => MonoTy.int('u16'));
const i32Ty = map(ident2('i32'), () => MonoTy.int('i32'));
const u32Ty = map(ident2('u32'), () => MonoTy.int('u32'));
const i64Ty = map(ident2('i64'), () => MonoTy.int('i64'));
const u64Ty = map(ident2('u64'), () => MonoTy.int('u64'));
const i128Ty = map(ident2('i128'), () => MonoTy.int('i128'));
const u128Ty = map(ident2('u128'), () => MonoTy.int('u128'));
const strTy = map(seq(ident2('str')), MonoTy.str);

const constTy = alt(
  voidTy, boolTy, strTy,
  u8Ty, i8Ty,
  u16Ty, i16Ty,
  u32Ty, i32Ty,
  u64Ty, i64Ty,
  u128Ty, i128Ty,
  parenthesizedTy,
);

const namedTy = alt(
  map(
    seq(
      alt(upperIdent, string('ptr')),
      optionalOrDefault(angleBrackets(commas(monoTy)), []),
    ),
    ([name, args]) => MonoTy.Const(name, ...args),
  ),
  constTy
);

const tupleTy = alt(
  map(
    parens(seq(
      monoTy,
      symbol(','),
      expectOrDefault(commas(monoTy), `Expected type in tuple type after ','`, []),
    )),
    ([h, _, tl]) => MonoTy.Tuple(Tuple.fromArray([h, ...tl]))
  ),
  namedTy
);

export const structTy = alt(
  map(
    curlyBrackets(commas(seq(
      ident,
      symbol(':'),
      expectOrDefault(monoTy, `Expected type in struct type after ':'`, MonoTy.Const('()')),
    ), true)),
    fields => MonoTy.Struct(Row.fromFields(fields.map(([name, _, ty]) => [name, ty]), false))
  ),
  tupleTy
);

const funTy = map(seq(
  alt(
    map(parens(optional(commas(monoTy))), args => args.orDefault([])),
    map(structTy, ty => [ty])
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
      structTy,
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

const variable = map(ident, (name, pos) => Expr.Variable(name, pos));

const block: Parser<Expr> = map(
  curlyBrackets(many(stmt2)),
  (stmts, pos) => {
    if (stmts.length > 0 && last(stmts).stmt.variant === 'Expr' && !last(stmts).discarded) {
      const [front, last] = deconsLast(stmts.map(proj('stmt')));
      return Expr.Block(front, some((last as VariantOf<Stmt, 'Expr'>).expr), pos);
    } else {
      return Expr.Block(stmts.map(proj('stmt')), none, pos);
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
  (lhs, [tyParams, args], pos) => Expr.Call(lhs, tyParams, args, pos)
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
    ([_l, h, _, tl], pos) => Expr.Tuple([h, ...tl], pos)
  ),
  app
);

const array = alt(
  map(squareBrackets(
    alt<ArrayInit>(
      map(seq(expr, symbol(';'), integerConst), ([value, _, count]) => ArrayInit.fill({ value, count: count.value })),
      map(commas(expr, true), elems => ArrayInit.elems({ elems })),
    )
  ), (init, pos) => Expr.Array(init, pos)),
  tuple
);

const structField = map(seq(
  ident,
  symbol(':'),
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
    ([name, params, fields], pos) => Expr.Struct(name, params, fields, pos)
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
  (lhs, _, [field, args], pos) => args.match({
    Some: args => match(field, {
      ident: ({ name }) => Expr.Call(Expr.Variable(name, pos), [], [lhs, ...args], pos),
      // this will fail in the inferencer as integers are not valid method names
      int: ({ value: n }) => Expr.Call(Expr.Variable(`${n}`, pos), [], [lhs, ...args], pos),
    }),
    None: () => match(field, {
      ident: ({ name }) => Expr.FieldAccess(lhs, name, pos),
      int: ({ value: n }) => Expr.TupleIndexing(lhs, n, pos),
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
    ([lhs, args, rhs], pos) => rhs.match({
      None: () => Expr.Call(Expr.Variable('[]', pos), [], [lhs, ...args], pos),
      Some: ([_, val]) => Expr.Call(Expr.Variable('[]=', pos), [], [lhs, ...args, val], pos),
    }),
  ),
  fieldAccess
);

const factor = indexing;

export const unary = alt(
  map(seq(
    string('-', 'not'),
    expectOrDefault(factor, `Expected expression after unary operator`, Expr.Error)
  ), ([op, expr], pos) => Expr.Call(Expr.Variable(op, pos), [], [expr], pos)),
  factor
);

const multiplicative = chainLeft(
  unary,
  string('*', '/', 'mod'),
  expect(unary, 'Expected expression after multiplicative operator'),
  (a, op, b, pos) => Expr.Call(Expr.Variable(op, pos), [], [a, b], pos)
);

const additive = chainLeft(
  multiplicative,
  string('+', '-'),
  expect(multiplicative, 'Expected expression after additive operator'),
  (a, op, b, pos) => Expr.Call(Expr.Variable(op, pos), [], [a, b], pos)
);

const comparison = chainLeft(
  additive,
  string('<=', '>=', '<', '>'),
  expect(additive, 'Expected expression after relational operator'),
  (a, op, b, pos) => Expr.Call(Expr.Variable(op, pos), [], [a, b], pos)
);

const and = chainLeft(
  comparison,
  string('and', 'nand'),
  expect(comparison, 'Expected expression after logical operator'),
  (a, op, b, pos) => Expr.Call(Expr.Variable(op, pos), [], [a, b], pos),
);

const or = chainLeft(
  and,
  string('or', 'nor', 'xor', 'xnor'),
  expect(and, 'Expected expression after logical operator'),
  (a, op, b, pos) => Expr.Call(Expr.Variable(op, pos), [], [a, b], pos),
);

const equality = chainLeft(
  or,
  string('==', '!='),
  expect(or, 'Expected expression after equality operator'),
  (a, op, b, pos) => Expr.Call(Expr.Variable(op, pos), [], [a, b], pos)
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
    ([_, cond, then, elseifs, else_], pos) => Expr.IfThenElse(cond, then, elseifs, else_.map(snd), pos)
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
    ([_let, pattern, ann, _eq, val, _in, body], pos) => Expr.LetIn(pattern, ann, val, body, pos)
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
    ([_, expr, ann, cases], pos) => Expr.Match(expr, ann, cases, pos)
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
    ([_l, args, _, body], pos) => Expr.Closure(args, body, pos)
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
  ([kw, name, ann, _, expr], pos) => Stmt.Let(name, expr, Token.eq(kw, Token.Keyword('mut', pos)), ann)
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
    ([lhs, op, rhs], pos) => {
      if (op === '=') {
        return Stmt.Assignment(lhs, rhs);
      } else {
        return Stmt.Assignment(lhs, Expr.Call(Expr.Variable(op.slice(0, -1), pos), [], [lhs, rhs], pos));
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

const outerAttributeList = map(seq(
  symbol('#'),
  alt(
    squareBrackets(commas(attribute)),
    map(attribute, attr => [attr])
  ),
),
  snd
);

const innerAttributeList = map(seq(
  symbol('#'),
  symbol('!'),
  alt(
    squareBrackets(commas(attribute)),
    map(attribute, attr => [attr])
  ),
),
  ([_1, _2, attrs]) => attrs,
);

const innerAttributeDecl = map(seq(
  symbol('#'),
  symbol('!'),
  alt(
    squareBrackets(commas(attribute)),
    map(attribute, attr => [attr])
  ),
),
  ([_1, _2, attrs]) => attrs,
);

const innerAttributesDecl: Parser<VariantOf<Decl, 'Attributes'>> = map(
  innerAttributeList,
  attrs => Decl.Attributes({ attributes: attrs })
);

const memberVisibility = optionalOrDefault(map(keyword('pub'), () => true), false);

const funDecl: Parser<VariantOf<Decl, 'Function'>> = map(seq(
  optionalOrDefault(outerAttributeList, []),
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
  innerAttributesDecl,
  funDecl,
  typeAliasDecl,
  importDecl,
));

export const parse = (source: string, path: string): [Decl[], Error[]] => {
  return lex(source, path).match({
    Ok: tokens => {
      lexerContext.tokens = tokens;
      const [res, _, errs] = consumeAll(many(decl)).ref(Slice.from(tokens));
      lexerContext.tokens = [];

      return res.match({
        Ok: decls => [decls, errs],
        Error: err => [[], [...errs, err]],
      });
    },
    Error: err => {
      return [[], [err]];
    },
  });
};
