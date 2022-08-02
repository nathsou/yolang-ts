import { match, VariantOf } from 'itsamatch';
import { Argument, Decl, Expr, Imports, Pattern, Prog, Stmt } from '../ast/sweet';
import { Inst } from '../codegen/wasm/instructions';
import { Row } from '../infer/records';
import { Tuple } from '../infer/tuples';
import { MonoTy, TypeParams } from '../infer/types';
import { deconsLast, last } from '../utils/array';
import { Either } from '../utils/either';
import { Maybe, none, some } from '../utils/maybe';
import { compose, ref, snd } from '../utils/misc';
import { error, ok, Result } from '../utils/result';
import { Slice } from '../utils/slice';
import { isLowerCase, isUpperCase } from '../utils/strings';
import { alt, angleBrackets, chainLeft, commas, consumeAll, curlyBrackets, expect, expectOrDefault, flatMap, initParser, keyword, leftAssoc, lookahead, many, map, mapParserResult, not, optional, optionalOrDefault, parens, Parser, ParserError, ParserResult, satisfy, satisfyBy, sepBy, seq, symbol, uninitialized } from './combinators';
import { Const, Token } from './token';

export const expr = uninitialized<Expr>();
const stmt = uninitialized<Stmt>();
const decl = uninitialized<Decl>();
const pattern = uninitialized<Pattern>();

// MISC

const ident = satisfyBy<string>(token => match(token, {
  Identifier: ({ name }) => isLowerCase(name[0]) ? some(name) : none,
  _: () => none
}));

const ident2 = (name: string) => satisfy(token => token.variant === 'Identifier' && token.name === name);

const stringIn = <S extends string>(names: Set<S>) => map(
  satisfy(token => token.variant === 'Identifier' && names.has(token.name as S)),
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

const modulePath = sepBy(symbol('.'))(upperIdent);
const typePath = map(sepBy(symbol('.'))(upperIdent), path => [path.slice(0, -1), last(path)] as const);

// TYPES
export const monoTy = uninitialized<MonoTy>();
const parenthesizedTy = map(parens(monoTy), ty => ty);
const unitTy = map(seq(symbol('('), symbol(')')), () => MonoTy.Const('()'));
const boolTy = map(ident2('bool'), () => MonoTy.Const('bool'));
const u32Ty = map(ident2('u32'), () => MonoTy.Const('u32'));
const constTy = alt(unitTy, boolTy, u32Ty);
const namedTy = map(
  seq(
    alt(
      map<[string, null], [string[], string]>(seq(upperIdent, not(symbol('.'))), ([name]) => [[], name]),
      map<string[], [string[], string]>(modulePath, path => [path.slice(0, -1), last(path)])
    ),
    optionalOrDefault(angleBrackets(optionalOrDefault(commas(monoTy), [])), []),
  ),
  ([[path, name], args]) => {
    if (path.length === 0 && args.length === 0) {
      return MonoTy.Const(name);
    } else {
      return MonoTy.ConstWithPath(path, name, ...args);
    }
  }
);

const tupleTy = map(
  parens(seq(
    monoTy,
    symbol(','),
    expectOrDefault(commas(monoTy), `Expected type in tuple type after ','`, []),
  )),
  ([h, _, tl]) => MonoTy.Tuple(Tuple.fromArray([h, ...tl]))
);

export const recordTy = map(
  curlyBrackets(optionalOrDefault(commas(seq(
    ident,
    symbol(':'),
    expectOrDefault(monoTy, `Expected type in record type after ':'`, MonoTy.Const('()')),
  )), [])),
  fields => MonoTy.Record(Row.fromFields(fields.map(([name, _, ty]) => [name, ty]), false))
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
    map(parens(optional(commas(monoTy))), args => args.orDefault([])),
    map(allExceptFunTy, ty => [ty])
  ),
  symbol('->'),
  monoTy,
),
  ([args, _, ret]) => MonoTy.Fun(args, ret)
);

initParser(monoTy, alt(
  funTy,
  allExceptFunTy,
));

const typeParams: Parser<TypeParams> = angleBrackets(commas(upperIdent));
const typeParamsInst: Parser<MonoTy[]> = angleBrackets(commas(monoTy));

const scopedTypeParams = <T>(p: Parser<T>): Parser<[TypeParams, T]> =>
  flatMap(optionalOrDefault(typeParams, []), params =>
    ref((tokens: Slice<Token>) =>
      map<T, [TypeParams, T]>(p, t => [params, t]).ref(tokens)
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

const integer = satisfyBy<number>(token =>
  token.variant === 'Const' &&
    token.value.variant === 'u32' ?
    some(token.value.value) :
    none
);

const boolConst = satisfyBy<Const>(token =>
  token.variant === 'Const' &&
    token.value.variant === 'bool' ?
    some(Const.bool(token.value.value)) :
    none
);

const integerConst = map(integer, Const.u32);

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

const bitwiseOp = alt(
  map(symbol('&'), () => '&' as const),
  map(symbol('|'), () => '|' as const),
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

const block: Parser<Expr> = map(
  curlyBrackets(many(stmt)),
  stmts => {
    if (stmts.length > 0 && last(stmts).variant === 'Expr') {
      const [front, last] = deconsLast(stmts);
      return Expr.Block(front, some((last as VariantOf<Stmt, 'Expr'>).expr));
    } else {
      return Expr.Block(stmts, none);
    }
  }
);

const wasmInst: Parser<Inst> = alt(
  map(seq(
    ident2('i32'),
    symbol('.'),
    map(
      stringIn(new Set([
        'add', 'sub', 'mul', 'div_s', 'div_u', 'rem_s', 'rem_u', 'and', 'or',
        'xor', 'shl', 'shr_s', 'shr_u', 'rotl', 'rotr', 'eqz', 'eq', 'ne',
        'lt_s', 'lt_u', 'le_s', 'le_u', 'gt_s', 'gt_u', 'ge_s', 'ge_u',
      ] as const)),
      inst => Inst.i32[inst]()
    )
  ), ([_1, _2, inst]) => inst),
);

export const wasmBlock: Parser<Expr> = map(
  seq(
    keyword('wasm'),
    curlyBrackets(
      commas(alt(
        map(wasmInst, inst => Either.left<Inst, [Expr, Maybe<MonoTy>]>(inst)),
        map(seq(
          expr,
          optional(typeAnnotation),
        ), ([expr, ann]) => Either.right<Inst, [Expr, Maybe<MonoTy>]>([expr, ann])),
      ))
    )
  ),
  ([_, insts]) => Expr.WasmBlock(insts)
);

const constExpr = map(constVal, Expr.Const);

export const parenthesized = map(parens(expr), Expr.Parenthesized);

export const primary = alt(
  constExpr,
  variable,
  parenthesized,
  block,
  wasmBlock,
  invalid,
  // unexpected
);

// e.g Main.Yolo.yo
const moduleAccess = alt(
  map(
    seq(
      modulePath,
      expect(symbol('.'), `Expected '.' after module path`),
      expectOrDefault(ident, `Expected identifier after '.'`, '<?>'),
    ),
    ([path, _, member]) => Expr.ModuleAccess(path, member),
  ),
  primary
);

const app = leftAssoc(
  moduleAccess,
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
    typePath,
    optionalOrDefault(angleBrackets(optionalOrDefault(commas(monoTy), [])), []),
    curlyBrackets(optionalOrDefault(commas(recordField), [])),
  ),
    ([[path, name], params, fields]) => Expr.NamedRecord(path, name, params, fields)
  ),
  tuple
);

// field access or method call or tuple indexing
const fieldAccess = chainLeft(
  namedRecord,
  symbol('.'),
  seq(
    expectOrDefault(alt<{ variant: 'ident', name: string } | { variant: 'int', value: number }>(
      map(ident, name => ({ variant: 'ident', name })),
      map(integer, n => ({ variant: 'int', value: n })),
    ), `Expected identifier or integer after '.'`, { variant: 'ident', name: '<?>' }),
  ),
  (lhs, _, [field]) => match(field, {
    ident: ({ name }) => Expr.FieldAccess(lhs, name),
    int: ({ value: n }) => Expr.TupleIndexing(lhs, n),
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
  (a, op, b) => Expr.BinaryOp(a, op, b)
);

const additive = chainLeft(
  multiplicative,
  additiveOp,
  expect(multiplicative, 'Expected expression after additive operator'),
  (a, op, b) => Expr.BinaryOp(a, op, b)
);

const relational = chainLeft(
  additive,
  relationalOp,
  expect(additive, 'Expected expression after relational operator'),
  (a, op, b) => Expr.BinaryOp(a, op, b)
);

const bitwise = chainLeft(
  relational,
  bitwiseOp,
  expect(relational, 'Expected expression after bitwise operator'),
  (a, op, b) => Expr.BinaryOp(a, op, b)
);

const equality = chainLeft(
  bitwise,
  equalityOp,
  expect(relational, 'Expected expression after equality operator'),
  (a, op, b) => Expr.BinaryOp(a, op, b)
);

const logical = chainLeft(
  equality,
  logicalOp,
  expect(equality, 'Expected expression after logical operator'),
  (a, op, b) => Expr.BinaryOp(a, op, b)
);

export const binaryExpr = logical;

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
  binaryExpr
);

const whileExpr = alt(
  map(
    seq(
      keyword('while'),
      expectOrDefault(expr, `Expected condition after 'while'`, Expr.Error),
      expectOrDefault(block, `Expected block after condidtion`, Expr.Block([])),
    ),
    ([_, cond, body]) => Expr.While(cond, body)
  ),
  ifThenElse
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
  whileExpr
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

const funcDecl: Parser<VariantOf<Decl, 'Function'>> = map(seq(
  keyword('fun'),
  expectOrDefault(ident, `Expected identifier after 'fn' keyword`, '<?>'),
  scopedTypeParams(seq(
    expectOrDefault(argumentList, 'Expected arguments after function name', []),
    optional(seq(
      symbol(':'),
      expect(monoTy, `Expected type after ':'`),
    )),
    expectOrDefault(block, 'Expected block after function arguments', Expr.Error),
  ))
),
  ([_, name, [typeParams, [args, returnTy, body]]]) => Decl.Function({ name, typeParams, args, returnTy: returnTy.map(snd), body })
);

const moduleDecl = map(seq(
  keyword('module'),
  expectOrDefault(upperIdent, `Expected an uppercase identifier after 'module' keyword`, '<?>'),
  curlyBrackets(many(decl)),
),
  ([_, name, decls]) => Decl.Module({ name, decls })
);

const typeAliasDecl: Parser<Decl> = map(seq(
  keyword('type'),
  expectOrDefault(upperIdent, `Expected identifier after 'type' keyword`, '<?>'),
  scopedTypeParams(seq(
    expectOrDefault(symbol('='), `Expected '=' after type name`, Token.Symbol('=')),
    monoTy,
  ))
),
  ([_t, name, [typeParams, [_eq, alias]]]) => Decl.TypeAlias({ name, typeParams, alias })
);

const useDecl = map(
  seq(
    keyword('use'),
    expect(modulePath, `Expected module path after 'use' keyword`),
    expect(symbol('.'), `Expected '.' after module path`),
    alt(
      map(curlyBrackets(commas(alt(upperIdent, ident))), Imports.names),
      map(symbol('*'), Imports.all),
    ),
    optional(symbol(';')),
  ),
  ([_, path, _dot, imports]) => Decl.Use({ path, imports })
);

initParser(decl, alt(
  funcDecl,
  typeAliasDecl,
  moduleDecl,
  useDecl,
));

export const parse = (tokens: Slice<Token>): [Prog, ParserError[]] => {
  const [res, _, errs] = consumeAll(many(decl)).ref(tokens);

  return res.match({
    Ok: decls => [decls, errs],
    Error: err => [[], [...errs, err]],
  });
};

export const parseRes = compose(parse, Result.wrap);