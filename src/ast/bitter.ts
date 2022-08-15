import { DataType, genConstructors, match } from "itsamatch";
import { Error } from "../errors/errors";
import { FuncDecl } from "../infer/env";
import { Tuple } from "../infer/tuples";
import { MonoTy, PolyTy, TypeParam } from "../infer/types";
import { Const } from "../parse/token";
import { Either } from "../utils/either";
import { Maybe, none, some } from "../utils/maybe";
import { FuncName, NameEnv, VarName } from "./name";
import { Argument as SweetArgument, Attribute, Decl as SweetDecl, Expr as SweetExpr, Imports, Pattern as SweetPattern, Prog as SweetProg, Stmt as SweetStmt } from "./sweet";

// Bitter expressions are *unsugared* representations
// of the structure of yolang source code
// with attached type information and lexically resolved identifier references

type WithSweetRefAndType<T> = {
  [K in keyof T]: T[K] & { sweet: SweetExpr, ty: MonoTy }
};

export type Pattern = DataType<{
  Const: { value: Const },
  Variable: { name: VarName },
  Tuple: { elements: Pattern[] },
  Any: {},
  Error: { message: string },
}>;

export const Pattern = {
  Const: (value: Const): Pattern => ({ variant: 'Const', value }),
  Variable: (name: VarName): Pattern => ({ variant: 'Variable', name }),
  Tuple: (elements: Pattern[]): Pattern => ({ variant: 'Tuple', elements }),
  Any: (): Pattern => ({ variant: 'Any' }),
  Error: (message: string): Pattern => ({ variant: 'Error', message }),
  fromSweet: (sweet: SweetPattern, nameEnv: NameEnv): Pattern => match(sweet, {
    Const: ({ value }): Pattern => Pattern.Const(value),
    Variable: ({ name }): Pattern => Pattern.Variable(NameEnv.declareVar(nameEnv, name, false)),
    Tuple: ({ elements }): Pattern => Pattern.Tuple(elements.map(e => Pattern.fromSweet(e, nameEnv))),
    Any: (): Pattern => Pattern.Any(),
    Error: ({ message }): Pattern => Pattern.Error(message),
  }),
  type: (pattern: Pattern): MonoTy => match(pattern, {
    Const: ({ value }) => Const.type(value),
    Variable: ({ name }) => name.ty,
    Tuple: ({ elements }) => MonoTy.Tuple(Tuple.fromArray(elements.map(Pattern.type))),
    Any: (): MonoTy => MonoTy.fresh(),
    Error: (): MonoTy => MonoTy.fresh(),
  }),
  vars: (pattern: Pattern): VarName[] => match(pattern, {
    Const: () => [],
    Variable: ({ name }) => [name],
    Tuple: ({ elements }) => elements.flatMap(Pattern.vars),
    Any: (): VarName[] => [],
    Error: (): VarName[] => [],
  }),
};

type Argument = { name: VarName, mutable: boolean, annotation: Maybe<MonoTy> };

export const Argument = {
  asMonoTy: ({ annotation }: Argument): MonoTy => annotation.orDefault(MonoTy.fresh),
};

export type Expr = DataType<WithSweetRefAndType<{
  Const: { value: Const },
  Variable: { name: VarName },
  NamedFuncCall: { name: Either<string, FuncName>, typeParams: MonoTy[], args: Expr[] },
  Call: { lhs: Expr, args: Expr[] },
  Error: { message: string },
  Closure: { args: Argument[], body: Expr },
  Block: { statements: Stmt[], lastExpr: Maybe<Expr> },
  IfThenElse: { condition: Expr, then: Expr, else_: Maybe<Expr> },
  Assignment: { lhs: Expr, rhs: Expr },
  FieldAccess: { lhs: Expr, field: string },
  Tuple: { elements: Expr[] },
  Match: { expr: Expr, annotation: Maybe<MonoTy>, cases: { pattern: Pattern, annotation: Maybe<MonoTy>, body: Expr }[] },
  Struct: { name: string, typeParams: MonoTy[], fields: { name: string, value: Expr }[] },
  TupleIndexing: { lhs: Expr, index: number },
  While: { condition: Expr, body: Expr },
}>>;

const typed = <T extends {}>(obj: T, sweet: SweetExpr): T & { ty: MonoTy, sweet: SweetExpr } => ({
  ...obj,
  sweet,
  ty: MonoTy.fresh(),
});

export const Expr = {
  Const: (c: Const, sweet: SweetExpr): Expr => ({ variant: 'Const', value: c, sweet, ty: Const.type(c) }),
  Variable: (name: VarName, sweet: SweetExpr): Expr => typed({ variant: 'Variable', name }, sweet),
  NamedFuncCall: (name: Either<string, FuncName>, typeParams: MonoTy[], args: Expr[], sweet: SweetExpr): Expr => typed({ variant: 'NamedFuncCall', name, typeParams, args }, sweet),
  Call: (lhs: Expr, args: Expr[], sweet: SweetExpr): Expr => typed({ variant: 'Call', lhs, args }, sweet),
  Error: (message: string, sweet: SweetExpr): Expr => ({ variant: 'Error', message, sweet, ty: MonoTy.unit() }),
  Closure: (args: Argument[], body: Expr, sweet: SweetExpr): Expr => typed({ variant: 'Closure', args, body }, sweet),
  Block: (statements: Stmt[], lastExpr: Maybe<Expr>, sweet: SweetExpr): Expr => typed({ variant: 'Block', statements, lastExpr }, sweet),
  IfThenElse: (condition: Expr, then: Expr, else_: Maybe<Expr>, sweet: SweetExpr): Expr => typed({ variant: 'IfThenElse', condition, then, else_ }, sweet),
  Assignment: (lhs: Expr, rhs: Expr, sweet: SweetExpr): Expr => typed({ variant: 'Assignment', lhs, rhs }, sweet),
  FieldAccess: (lhs: Expr, field: string, sweet: SweetExpr): Expr => typed({ variant: 'FieldAccess', lhs, field }, sweet),
  Tuple: (elements: Expr[], sweet: SweetExpr): Expr => typed({ variant: 'Tuple', elements }, sweet),
  Match: (expr: Expr, annotation: Maybe<MonoTy>, cases: { pattern: Pattern, annotation: Maybe<MonoTy>, body: Expr }[], sweet: SweetExpr): Expr => typed({ variant: 'Match', expr, annotation, cases }, sweet),
  Struct: (name: string, typeParams: MonoTy[], fields: { name: string, value: Expr }[], sweet: SweetExpr): Expr => typed({ variant: 'Struct', name, typeParams, fields }, sweet),
  TupleIndexing: (lhs: Expr, index: number, sweet: SweetExpr): Expr => typed({ variant: 'TupleIndexing', lhs, index }, sweet),
  While: (condition: Expr, body: Expr, sweet: SweetExpr): Expr => typed({ variant: 'While', condition, body }, sweet),
  fromSweet: (sweet: SweetExpr, nameEnv: NameEnv, errors: Error[]): Expr => {
    const go = (expr: SweetExpr, env = nameEnv) => Expr.fromSweet(expr, env, errors);

    return match(sweet, {
      Const: ({ value }) => Expr.Const(value, sweet),
      Variable: ({ name }) => Expr.Variable(NameEnv.resolveVar(nameEnv, name), sweet),
      Call: ({ lhs, typeParams, args }) => {
        return match(lhs, {
          Variable: ({ name }) => Expr.NamedFuncCall(Either.left(name), typeParams, args.map(arg => go(arg)), sweet),
          _: () => Expr.Call(go(lhs), args.map(arg => go(arg)), sweet),
        });
      },
      Error: ({ message }) => Expr.Error(message, sweet),
      Closure: ({ args, body }) => {
        const withoutPatterns = rewriteFuncArgsPatternMatching(args, body, nameEnv, errors);
        return Expr.Closure(
          withoutPatterns.args,
          withoutPatterns.body,
          sweet
        );
      },
      Block: ({ statements, lastExpr }) => {
        const newNameEnv = NameEnv.clone(nameEnv);
        return Expr.Block(statements.map(s => Stmt.fromSweet(s, newNameEnv, errors)), lastExpr.map(e => go(e, newNameEnv)), sweet);
      },
      IfThenElse: ({ condition, then, else_ }) => Expr.IfThenElse(go(condition), go(then), else_.map(go), sweet),
      Assignment: ({ lhs, rhs }) => Expr.Assignment(go(lhs), go(rhs), sweet),
      FieldAccess: ({ lhs, field }) => Expr.FieldAccess(go(lhs), field, sweet),
      Tuple: ({ elements }) => Expr.Tuple(elements.map(e => go(e)), sweet),
      Match: ({ expr, annotation, cases }) => Expr.Match(
        go(expr),
        annotation,
        cases.map(c => {
          const bodyEnv = NameEnv.clone(nameEnv);
          return {
            pattern: Pattern.fromSweet(c.pattern, bodyEnv),
            annotation: c.annotation,
            body: go(c.body, bodyEnv)
          };
        }),
        sweet
      ),
      Parenthesized: ({ expr }) => go(expr),
      Struct: ({ name, typeParams, fields }) => Expr.Struct(name, typeParams, fields.map(f => ({ name: f.name, value: go(f.value) })), sweet),
      TupleIndexing: ({ lhs, index }) => Expr.TupleIndexing(go(lhs), index, sweet),
      LetIn: ({ pattern, annotation, value, body }) => {
        // let pat = v in b --> match v with { pat => body }
        return Expr.Match(
          go(value),
          none,
          [{ pattern: Pattern.fromSweet(pattern, nameEnv), annotation, body: go(body) }],
          sweet
        );
      },
      While: ({ condition, body }) => Expr.While(go(condition), go(body), sweet),
    });
  },
  showSweet: (expr: Expr): string => SweetExpr.show(expr.sweet),
  rewrite: (expr: Expr, nameEnv: NameEnv, rewriteExpr: (expr: Expr) => Expr): Expr => {
    const go = (expr: Expr): Expr => Expr.rewrite(expr, nameEnv, rewriteExpr);

    return rewriteExpr(match(expr, {
      Const: ({ value }) => Expr.Const(value, expr.sweet),
      Variable: ({ name }) => Expr.Variable(NameEnv.resolveVar(nameEnv, name.original), expr.sweet),
      NamedFuncCall: ({ name, typeParams, args }) => Expr.NamedFuncCall(name, typeParams, args.map(arg => go(arg)), expr.sweet),
      Call: ({ lhs, args }) => Expr.Call(go(lhs), args.map(arg => go(arg)), expr.sweet),
      Error: ({ message }) => Expr.Error(message, expr.sweet),
      Closure: ({ args, body }) => Expr.Closure(args.map(arg => ({ ...arg, name: NameEnv.resolveVar(nameEnv, arg.name.original) })), go(body), expr.sweet),
      Block: ({ statements, lastExpr }) => Expr.Block(statements.map(s => Stmt.rewrite(s, nameEnv, rewriteExpr)), lastExpr.map(e => go(e)), expr.sweet),
      IfThenElse: ({ condition, then, else_ }) => Expr.IfThenElse(go(condition), go(then), else_.map(go), expr.sweet),
      Assignment: ({ lhs, rhs }) => Expr.Assignment(go(lhs), go(rhs), expr.sweet),
      FieldAccess: ({ lhs, field }) => Expr.FieldAccess(go(lhs), field, expr.sweet),
      Tuple: ({ elements }) => Expr.Tuple(elements.map(e => go(e)), expr.sweet),
      Match: ({ expr, annotation, cases }) => Expr.Match(go(expr), annotation, cases.map(c => ({ ...c, body: go(c.body) })), expr.sweet),
      Struct: ({ name, typeParams, fields }) => Expr.Struct(name, typeParams, fields.map(f => ({ ...f, value: go(f.value) })), expr.sweet),
      TupleIndexing: ({ lhs, index }) => Expr.TupleIndexing(go(lhs), index, expr.sweet),
      While: ({ condition, body }) => Expr.While(go(condition), go(body), expr.sweet),
    }));
  },
  isMutable: (expr: Expr): boolean => match(expr, {
    Variable: ({ name }) => name.mutable,
    FieldAccess: ({ lhs }) => Expr.isMutable(lhs),
    Block: ({ lastExpr }) => lastExpr.mapWithDefault(Expr.isMutable, false),
    IfThenElse: ({ then, else_ }) => else_.mapWithDefault(Expr.isMutable, false) && Expr.isMutable(then),
    _: () => false,
  }),
};

export type Stmt = DataType<{
  Let: { name: VarName, expr: Expr, mutable: boolean, annotation: Maybe<MonoTy> },
  Expr: { expr: Expr },
  Error: { message: string },
}>;

export const Stmt = {
  Let: (name: VarName, expr: Expr, mutable: boolean, annotation: Maybe<MonoTy>): Stmt => ({ variant: 'Let', name, expr, mutable, annotation }),
  Expr: (expr: Expr): Stmt => ({ variant: 'Expr', expr }),
  Error: (message: string): Stmt => ({ variant: 'Error', message }),
  fromSweet: (sweet: SweetStmt, nameEnv: NameEnv, errors: Error[]): Stmt => {
    return match(sweet, {
      Let: ({ name, expr, mutable, annotation }) => Stmt.Let(
        NameEnv.declareVar(nameEnv, name, mutable),
        Expr.fromSweet(expr, nameEnv, errors),
        mutable,
        annotation
      ),
      Expr: ({ expr }) => Stmt.Expr(Expr.fromSweet(expr, nameEnv, errors)),
      Error: ({ message }) => Stmt.Error(message),
    });
  },
  rewrite: (stmt: Stmt, nameEnv: NameEnv, f: (expr: Expr) => Expr): Stmt => {
    return match(stmt, {
      Let: ({ name, expr, mutable, annotation }) => Stmt.Let(
        NameEnv.resolveVar(nameEnv, name.original),
        Expr.rewrite(expr, nameEnv, f),
        mutable,
        annotation
      ),
      Expr: ({ expr }) => Stmt.Expr(Expr.rewrite(expr, nameEnv, f)),
      Error: ({ message }) => Stmt.Error(message),
    });
  },
};

type FuncArg = { name: VarName, mutable: boolean, annotation: Maybe<MonoTy> };

export type Decl = DataType<{
  Function: {
    attributes: Attribute[],
    pub: boolean,
    name: FuncName,
    typeParams: { name: string, ty: Maybe<MonoTy> }[],
    args: FuncArg[],
    returnTy: Maybe<MonoTy>,
    body: Maybe<Expr>,
    funTy: PolyTy,
    instances: FuncDecl[],
  },
  TypeAlias: { name: string, typeParams: TypeParam[], alias: MonoTy },
  Import: { path: string, imports: Imports },
  Error: { message: string },
}>;

const { TypeAlias, Import } = genConstructors<Decl>(['TypeAlias', 'Import']);

type FuncConstructorParams = {
  attributes: Attribute[],
  pub: boolean,
  name: FuncName,
  typeParams: { name: string, ty: Maybe<MonoTy> }[],
  args: FuncArg[],
  returnTy: Maybe<MonoTy>,
  body: Maybe<Expr>,
};

export const Decl = {
  Function: ({ attributes, pub, name, typeParams, args, returnTy, body }: FuncConstructorParams): Decl => ({
    variant: 'Function',
    attributes,
    pub,
    name,
    typeParams,
    args,
    body,
    returnTy,
    funTy: MonoTy.toPoly(
      MonoTy.Fun(
        args.map(a => a.annotation.orDefault(MonoTy.fresh)),
        returnTy.orDefault(MonoTy.fresh)
      )
    ),
    instances: [],
  }),
  TypeAlias,
  Import,
  Error: (message: string): Decl => ({ variant: 'Error', message }),
  fromSweet: (sweet: SweetDecl, nameEnv: NameEnv, moduleStack: string[], errors: Error[]): Decl =>
    match(sweet, {
      Function: ({ attributes, pub, name, typeParams, args, returnTy, body }) => {
        const nameRef = NameEnv.declareFunc(nameEnv, name);
        const withoutPatterns = rewriteFuncArgsPatternMatching(args, body.orDefault(SweetExpr.Block([], none)), nameEnv, errors);

        return Decl.Function({
          attributes,
          pub,
          name: nameRef,
          typeParams,
          args: withoutPatterns.args,
          returnTy,
          body: body.match({
            Some: () => some(withoutPatterns.body),
            None: () => none,
          }),
        });
      },
      TypeAlias: ({ name, typeParams, alias }) => Decl.TypeAlias({ name, typeParams, alias }),
      Import: ({ path, imports }) => Decl.Import({ path, imports }),
      Error: ({ message }) => Decl.Error(message),
    }),
  rewrite: (decl: Decl, nameEnv: NameEnv, rewriteExpr: (expr: Expr) => Expr, rewriteDecl: (decl: Decl) => Decl): Decl => {
    return match(decl, {
      Function: ({ attributes, pub, name, typeParams, args, body, returnTy }) => {
        const bodyEnv = NameEnv.clone(nameEnv);
        return Decl.Function({
          attributes,
          pub,
          name: NameEnv.declareFunc(nameEnv, name.original, name.mangled),
          typeParams: typeParams.map(p => ({ name: p.name, ty: none })),
          args: args.map(arg => ({ ...arg, name: NameEnv.declareVar(bodyEnv, arg.name.original, false, arg.name.mangled) })),
          returnTy,
          body: body.map(b => Expr.rewrite(b, bodyEnv, rewriteExpr)),
        });
      },
      TypeAlias: ({ name, typeParams, alias }) => Decl.TypeAlias({ name, typeParams, alias }),
      Import: ({ path, imports }) => Decl.Import({ path, imports }),
      Error: ({ message }) => Decl.Error(message),
    });
  },
};

export type Prog = Decl[];

export type BitterConversionError = {
  message: string,
};

export const Prog = {
  fromSweet: (prog: SweetProg): [prog: Prog, errors: Error[]] => {
    const nameEnv = NameEnv.make();
    const errors: Error[] = [];
    const bitterProg = prog.map(decl => Decl.fromSweet(
      decl,
      nameEnv,
      [],
      errors
    ));

    return [bitterProg, errors];
  },
  rewrite: (prog: Prog, nameEnv: NameEnv, rewriteExpr: (expr: Expr) => Expr, rewriteDecl: (decl: Decl) => Decl): Prog => {
    return prog.map(decl => Decl.rewrite(decl, nameEnv, rewriteExpr, rewriteDecl));
  },
};

export const rewriteFuncArgsPatternMatching = (
  args: SweetArgument[],
  body: SweetExpr,
  nameEnv: NameEnv,
  errors: Error[],
): { args: Argument[], body: Expr } => {
  const bodyEnv = NameEnv.clone(nameEnv);
  const firstRefuttableArg = args.find(({ pattern }) => !SweetPattern.isIrrefutable(pattern));
  if (firstRefuttableArg !== undefined) {
    errors.push(Error.BitterConversion({
      message: `patterns in function arguments must be irrefutable, but ${SweetPattern.show(firstRefuttableArg.pattern)} is not`,
    }));
  }

  // we don't have to do anything if all the patterns are variables or _
  if (args.every(arg => arg.pattern.variant === 'Variable' || arg.pattern.variant === 'Any')) {
    const declaredArgs = args.map(({ pattern, mutable, annotation }, index) => ({
      name: NameEnv.declareVar(bodyEnv, pattern.variant === 'Variable' ? pattern.name : `_${index}`, mutable),
      mutable,
      annotation
    }));

    return {
      args: declaredArgs,
      body: Expr.fromSweet(body, bodyEnv, errors),
    };
  }

  // transform (p1, p2, ...) -> body into (arg1, arg2, ...) -> match { (p1, p2, ...) => body }
  // where arg1, arg2, ... are variables
  const varNames = args.map(({ mutable }, index) => NameEnv.declareVar(bodyEnv, `arg${index}`, mutable));
  const matchedExpr = args.length === 0 ?
    SweetExpr.Variable(varNames[0].original) :
    SweetExpr.Tuple(varNames.map(name => SweetExpr.Variable(name.original)));

  const pattern = args.length === 0 ?
    args[0].pattern :
    SweetPattern.Tuple(args.map(({ pattern }) => pattern));


  const newBody = Expr.fromSweet(
    SweetExpr.Match(matchedExpr, none, [{ pattern, annotation: none, body }]),
    bodyEnv,
    errors
  );

  return {
    args: varNames.map(arg => ({ name: arg, mutable: arg.mutable, annotation: none })),
    body: newBody,
  };
};