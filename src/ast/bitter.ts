import { DataType, match, VariantOf } from "itsamatch";
import { Inst } from "../codegen/wasm/instructions";
import { Error } from "../errors/errors";
import { Tuple } from "../infer/tuples";
import { TypeContext } from "../infer/typeContext";
import { MonoTy, PolyTy, TypeParams } from "../infer/types";
import { Const } from "../parse/token";
import { Either } from "../utils/either";
import { Maybe, none } from "../utils/maybe";
import { id } from "../utils/misc";
import { Name, NameEnv } from "./name";
import { Argument as SweetArgument, BinaryOperator, CompoundAssignmentOperator, Decl as SweetDecl, Expr as SweetExpr, Imports, Pattern as SweetPattern, Prog as SweetProg, Stmt as SweetStmt, UnaryOperator } from "./sweet";

// Bitter expressions are *unsugared* representations
// of the structure of yolang source code
// with attached type information and lexically resolved identifier references

type WithSweetRefAndType<T> = {
  [K in keyof T]: T[K] & { sweet: SweetExpr, ty: MonoTy, typeContext?: TypeContext }
};

export type Pattern = DataType<{
  Const: { value: Const },
  Variable: { name: Name },
  Tuple: { elements: Pattern[] },
  Any: {},
  Error: { message: string },
}>;

export const Pattern = {
  Const: (value: Const): Pattern => ({ variant: 'Const', value }),
  Variable: (name: Name): Pattern => ({ variant: 'Variable', name }),
  Tuple: (elements: Pattern[]): Pattern => ({ variant: 'Tuple', elements }),
  Any: (): Pattern => ({ variant: 'Any' }),
  Error: (message: string): Pattern => ({ variant: 'Error', message }),
  fromSweet: (sweet: SweetPattern, nameEnv: NameEnv): Pattern => match(sweet, {
    Const: ({ value }): Pattern => Pattern.Const(value),
    Variable: ({ name }): Pattern => Pattern.Variable(NameEnv.declare(nameEnv, name, false)),
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
  vars: (pattern: Pattern): Name[] => match(pattern, {
    Const: () => [],
    Variable: ({ name }) => [name],
    Tuple: ({ elements }) => elements.flatMap(Pattern.vars),
    Any: (): Name[] => [],
    Error: (): Name[] => [],
  }),
};

type Argument = { name: Name, mutable: boolean, annotation: Maybe<MonoTy> };

export const Argument = {
  asMonoTy: ({ annotation }: Argument): MonoTy => annotation.orDefault(MonoTy.fresh),
};

export type Expr = DataType<WithSweetRefAndType<{
  Const: { value: Const },
  Variable: { name: Name },
  NamedFuncCall: { name: Name, typeParams: MonoTy[], args: Expr[] },
  Call: { lhs: Expr, args: Expr[] },
  BinaryOp: { lhs: Expr, op: BinaryOperator, rhs: Expr },
  UnaryOp: { op: UnaryOperator, expr: Expr },
  Error: { message: string },
  Closure: { args: Argument[], body: Expr },
  Block: { statements: Stmt[], lastExpr: Maybe<Expr> },
  IfThenElse: { condition: Expr, then: Expr, else_: Maybe<Expr> },
  Assignment: { lhs: Expr, rhs: Expr },
  ModuleAccess: { path: string[], member: string },
  FieldAccess: { lhs: Expr, field: string },
  Tuple: { elements: Expr[] },
  Match: { expr: Expr, annotation: Maybe<MonoTy>, cases: { pattern: Pattern, annotation: Maybe<MonoTy>, body: Expr }[] },
  NamedRecord: { path: string[], name: string, typeParams: MonoTy[], fields: { name: string, value: Expr }[] },
  TupleIndexing: { lhs: Expr, index: number },
  WasmBlock: { instructions: Either<Inst, [Expr, Maybe<MonoTy>]>[] },
  While: { condition: Expr, body: Expr },
}>>;

const typed = <T extends {}>(obj: T, sweet: SweetExpr): T & { ty: MonoTy, sweet: SweetExpr } => ({
  ...obj,
  sweet,
  ty: MonoTy.fresh(),
});

export const Expr = {
  Const: (c: Const, sweet: SweetExpr): Expr => ({ variant: 'Const', value: c, sweet, ty: Const.type(c) }),
  Variable: (name: Name, sweet: SweetExpr): Expr => typed({ variant: 'Variable', name }, sweet),
  NamedFuncCall: (name: Name, typeParams: MonoTy[], args: Expr[], sweet: SweetExpr): Expr => typed({ variant: 'NamedFuncCall', name, typeParams, args }, sweet),
  Call: (lhs: Expr, args: Expr[], sweet: SweetExpr): Expr => typed({ variant: 'Call', lhs, args }, sweet),
  BinaryOp: (lhs: Expr, op: BinaryOperator, rhs: Expr, sweet: SweetExpr): Expr => typed({ variant: 'BinaryOp', lhs, op, rhs }, sweet),
  UnaryOp: (op: UnaryOperator, expr: Expr, sweet: SweetExpr): Expr => typed({ variant: 'UnaryOp', op, expr }, sweet),
  Error: (message: string, sweet: SweetExpr): Expr => ({ variant: 'Error', message, sweet, ty: MonoTy.unit() }),
  Closure: (args: Argument[], body: Expr, sweet: SweetExpr): Expr => typed({ variant: 'Closure', args, body }, sweet),
  Block: (statements: Stmt[], lastExpr: Maybe<Expr>, sweet: SweetExpr): Expr => typed({ variant: 'Block', statements, lastExpr }, sweet),
  IfThenElse: (condition: Expr, then: Expr, else_: Maybe<Expr>, sweet: SweetExpr): Expr => typed({ variant: 'IfThenElse', condition, then, else_ }, sweet),
  Assignment: (lhs: Expr, rhs: Expr, sweet: SweetExpr): Expr => typed({ variant: 'Assignment', lhs, rhs }, sweet),
  ModuleAccess: (path: string[], member: string, sweet: SweetExpr): Expr => typed({ variant: 'ModuleAccess', path, member }, sweet),
  FieldAccess: (lhs: Expr, field: string, sweet: SweetExpr): Expr => typed({ variant: 'FieldAccess', lhs, field }, sweet),
  Tuple: (elements: Expr[], sweet: SweetExpr): Expr => typed({ variant: 'Tuple', elements }, sweet),
  Match: (expr: Expr, annotation: Maybe<MonoTy>, cases: { pattern: Pattern, annotation: Maybe<MonoTy>, body: Expr }[], sweet: SweetExpr): Expr => typed({ variant: 'Match', expr, annotation, cases }, sweet),
  NamedRecord: (path: string[], name: string, typeParams: MonoTy[], fields: { name: string, value: Expr }[], sweet: SweetExpr): Expr => typed({ variant: 'NamedRecord', path, name, typeParams, fields }, sweet),
  TupleIndexing: (lhs: Expr, index: number, sweet: SweetExpr): Expr => typed({ variant: 'TupleIndexing', lhs, index }, sweet),
  WasmBlock: (instructions: Either<Inst, [Expr, Maybe<MonoTy>]>[], sweet: SweetExpr): Expr => typed({ variant: 'WasmBlock', instructions }, sweet),
  While: (condition: Expr, body: Expr, sweet: SweetExpr): Expr => typed({ variant: 'While', condition, body }, sweet),
  fromSweet: (sweet: SweetExpr, nameEnv: NameEnv, errors: Error[]): Expr => {
    const go = (expr: SweetExpr, env = nameEnv) => Expr.fromSweet(expr, env, errors);

    return match(sweet, {
      Const: ({ value }) => Expr.Const(value, sweet),
      Variable: ({ name }) => Expr.Variable(NameEnv.resolve(nameEnv, name), sweet),
      Call: ({ lhs, typeParams, args }) => match(lhs, {
        Variable: ({ name }) => Expr.NamedFuncCall(NameEnv.resolve(nameEnv, name), typeParams, args.map(arg => go(arg)), sweet),
        _: () => Expr.Call(go(lhs), args.map(arg => go(arg)), sweet)
      }),
      BinaryOp: ({ lhs, op, rhs }) => Expr.BinaryOp(go(lhs), op, go(rhs), sweet),
      UnaryOp: ({ op, expr }) => Expr.UnaryOp(op, go(expr), sweet),
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
      CompoundAssignment: ({ lhs, op, rhs }): Expr => {
        const opMap: Record<CompoundAssignmentOperator, BinaryOperator> = {
          '+=': '+',
          '-=': '-',
          '*=': '*',
          '/=': '/',
          '%=': '%',
          '&=': '&',
          '|=': '|',
          '&&=': '&&',
          '||=': '||',
        };

        // syntactic sugar for: lhs = lhs op rhs
        const bitterLhs = go(lhs);
        return Expr.Assignment(
          bitterLhs,
          Expr.BinaryOp(
            bitterLhs,
            opMap[op],
            go(rhs),
            sweet
          ),
          sweet
        );
      },
      ModuleAccess: ({ path, member }) => Expr.ModuleAccess(path, member, sweet),
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
      NamedRecord: ({ path, name, typeParams, fields }) => Expr.NamedRecord(path, name, typeParams, fields.map(f => ({ name: f.name, value: go(f.value) })), sweet),
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
      WasmBlock: ({ instructions }) => Expr.WasmBlock(instructions.map(i => i.map({ left: id, right: ([expr, ty]) => [go(expr), ty] })), sweet),
      While: ({ condition, body }) => Expr.While(go(condition), go(body), sweet),
    });
  },
  showSweet: (expr: Expr): string => SweetExpr.show(expr.sweet),
  rewrite: (expr: Expr, nameEnv: NameEnv, rewriteExpr: (expr: Expr) => Expr): Expr => {
    const go = (expr: Expr): Expr => Expr.rewrite(expr, nameEnv, rewriteExpr);

    const newExpr = match(expr, {
      Const: ({ value }) => Expr.Const(value, expr.sweet),
      Variable: ({ name }) => Expr.Variable(NameEnv.resolve(nameEnv, name.original, name.renaming), expr.sweet),
      NamedFuncCall: ({ name, typeParams, args }) => Expr.NamedFuncCall(name, typeParams, args.map(arg => go(arg)), expr.sweet),
      Call: ({ lhs, args }) => Expr.Call(go(lhs), args.map(arg => go(arg)), expr.sweet),
      BinaryOp: ({ lhs, op, rhs }) => Expr.BinaryOp(go(lhs), op, go(rhs), expr.sweet),
      UnaryOp: ({ op, expr }) => Expr.UnaryOp(op, go(expr), expr.sweet),
      Error: ({ message }) => Expr.Error(message, expr.sweet),
      Closure: ({ args, body }) => Expr.Closure(args.map(arg => ({ ...arg, name: NameEnv.resolve(nameEnv, arg.name.original, arg.name.renaming) })), go(body), expr.sweet),
      Block: ({ statements, lastExpr }) => Expr.Block(statements.map(s => Stmt.rewrite(s, nameEnv, rewriteExpr)), lastExpr.map(e => go(e)), expr.sweet),
      IfThenElse: ({ condition, then, else_ }) => Expr.IfThenElse(go(condition), go(then), else_.map(go), expr.sweet),
      Assignment: ({ lhs, rhs }) => Expr.Assignment(go(lhs), go(rhs), expr.sweet),
      ModuleAccess: ({ path, member }) => Expr.ModuleAccess(path, member, expr.sweet),
      FieldAccess: ({ lhs, field }) => Expr.FieldAccess(go(lhs), field, expr.sweet),
      Tuple: ({ elements }) => Expr.Tuple(elements.map(e => go(e)), expr.sweet),
      Match: ({ expr, annotation, cases }) => Expr.Match(go(expr), annotation, cases.map(c => ({ ...c, body: go(c.body) })), expr.sweet),
      NamedRecord: ({ path, name, typeParams, fields }) => Expr.NamedRecord(path, name, typeParams, fields.map(f => ({ ...f, value: go(f.value) })), expr.sweet),
      TupleIndexing: ({ lhs, index }) => Expr.TupleIndexing(go(lhs), index, expr.sweet),
      WasmBlock: ({ instructions }) => Expr.WasmBlock(instructions.map(i => i.map({ left: id, right: ([expr, ty]) => [go(expr), ty] })), expr.sweet),
      While: ({ condition, body }) => Expr.While(go(condition), go(body), expr.sweet),
    });

    newExpr.typeContext = expr.typeContext;
    return rewriteExpr(newExpr);
  },
};

export type Stmt = DataType<{
  Let: { name: Name, expr: Expr, mutable: boolean, annotation: Maybe<MonoTy> },
  Expr: { expr: Expr },
  Error: { message: string },
}>;

export const Stmt = {
  Let: (name: Name, expr: Expr, mutable: boolean, annotation: Maybe<MonoTy>): Stmt => ({ variant: 'Let', name, expr, mutable, annotation }),
  Expr: (expr: Expr): Stmt => ({ variant: 'Expr', expr }),
  Error: (message: string): Stmt => ({ variant: 'Error', message }),
  fromSweet: (sweet: SweetStmt, nameEnv: NameEnv, errors: Error[]): Stmt => {
    return match(sweet, {
      Let: ({ name, expr, mutable, annotation }) => Stmt.Let(
        NameEnv.declare(nameEnv, name, mutable),
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
        NameEnv.resolve(nameEnv, name.original),
        Expr.rewrite(expr, nameEnv, f),
        mutable,
        annotation
      ),
      Expr: ({ expr }) => Stmt.Expr(Expr.rewrite(expr, nameEnv, f)),
      Error: ({ message }) => Stmt.Error(message),
    });
  },
};

type WithTypeContext<T> = {
  [K in keyof T]: T[K] & { typeContext?: TypeContext }
};

export type Decl = DataType<WithTypeContext<{
  Function: {
    name: Name,
    typeParams: TypeParams,
    args: { name: Name, mutable: boolean, annotation: Maybe<MonoTy> }[],
    returnTy: Maybe<MonoTy>,
    body: Expr,
    funTy: PolyTy,
  },
  Module: { name: string, decls: Decl[], members: Record<string, Decl> },
  TypeAlias: { name: string, typeParams: TypeParams, alias: MonoTy },
  Use: { path: string[], imports: Imports },
  Error: { message: string },
}>>;

export const Decl = {
  Function: (
    name: Name,
    typeParams: TypeParams,
    args: { name: Name, mutable: boolean, annotation: Maybe<MonoTy> }[],
    returnTy: Maybe<MonoTy>,
    body: Expr
  ): Decl => ({
    variant: 'Function',
    name,
    typeParams,
    args,
    body,
    returnTy,
    funTy: PolyTy.fresh()
  }),
  Module: (name: string, decls: Decl[]): Decl => {
    const mod: VariantOf<Decl, 'Module'> = {
      variant: 'Module',
      name,
      decls,
      members: {},
    };

    for (const decl of decls) {
      match(decl, {
        Function: func => {
          mod.members[func.name.original] = func;
        },
        Module: subMod => {
          mod.members[subMod.name] = subMod;
        },
        TypeAlias: alias => {
          mod.members[alias.name] = alias;
        },
        Use: () => { },
        Error: () => { },
      });
    }

    return mod;
  },
  TypeAlias: (name: string, typeParams: TypeParams, alias: MonoTy): Decl => ({ variant: 'TypeAlias', name, typeParams, alias }),
  Use: (path: string[], imports: Imports): Decl => ({ variant: 'Use', path, imports }),
  Error: (message: string): Decl => ({ variant: 'Error', message }),
  fromSweet: (sweet: SweetDecl, nameEnv: NameEnv, declareFuncNames: boolean, moduleStack: string[], errors: Error[]): Decl =>
    match(sweet, {
      Function: ({ name, typeParams, args, returnTy, body }) => {
        const withoutPatterns = rewriteFuncArgsPatternMatching(args, body, nameEnv, errors);
        const nameRef: Name = declareFuncNames ? NameEnv.declare(nameEnv, name, false) : NameEnv.resolve(nameEnv, name);
        nameRef.renaming = [...moduleStack, name].join('_');

        return Decl.Function(
          nameRef,
          typeParams,
          withoutPatterns.args,
          returnTy,
          withoutPatterns.body
        );
      },
      Module: ({ name, decls }) => {
        const modEnv = NameEnv.clone(nameEnv);

        for (const decl of decls) {
          if (decl.variant === 'Function') {
            NameEnv.declare(modEnv, decl.name, false);
          }
        }

        return Decl.Module(
          name,
          decls.map(decl => Decl.fromSweet(decl, modEnv, false, [...moduleStack, name], errors))
        );
      },
      TypeAlias: ({ name, typeParams, alias }) => Decl.TypeAlias(name, typeParams, alias),
      Use: ({ path, imports }) => Decl.Use(path, imports),
      Error: ({ message }) => Decl.Error(message),
    }),
  rewrite: (decl: Decl, nameEnv: NameEnv, rewriteExpr: (expr: Expr) => Expr, rewriteDecl: (decl: Decl) => Decl): Decl => {
    const go = (decl: Decl): Decl => Decl.rewrite(decl, nameEnv, rewriteExpr, rewriteDecl);

    const newDecl = match(decl, {
      Function: ({ name, typeParams, args, body, returnTy }) => Decl.Function(
        NameEnv.resolve(nameEnv, name.original, name.renaming),
        typeParams,
        args.map(arg => ({ ...arg, name: NameEnv.resolve(nameEnv, arg.name.original, arg.name.renaming) })),
        returnTy,
        Expr.rewrite(body, nameEnv, rewriteExpr),
      ),
      Module: ({ name, decls }) => Decl.Module(name, decls.map(decl => go(decl))),
      TypeAlias: ({ name, typeParams, alias }) => Decl.TypeAlias(name, typeParams, alias),
      Use: ({ path, imports }) => Decl.Use(path, imports),
      Error: ({ message }) => Decl.Error(message),
    });

    newDecl.typeContext = decl.typeContext;
    return rewriteDecl(newDecl);
  },
};

export type Prog = Decl[];

export type BitterConversionError = {
  message: string,
};

export const Prog = {
  fromSweet: (prog: SweetProg): [prog: Prog, errors: Error[]] => {
    const nameEnv = NameEnv.make();

    // declare all function names beforehand
    // so that they can be used anywhere in the program
    for (const decl of prog) {
      if (decl.variant === 'Function') {
        NameEnv.declare(nameEnv, decl.name, false);
      }
    }

    const errors: Error[] = [];

    const bitterProg = prog.map(decl => Decl.fromSweet(
      decl,
      nameEnv,
      false, // do not redeclare function names
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
  const bodyEnv = NameEnv.make();
  const firstRefuttableArg = args.find(({ pattern }) => !SweetPattern.isIrrefutable(pattern));
  if (firstRefuttableArg !== undefined) {
    errors.push(Error.BitterConversion({
      message: `patterns in function arguments must be irrefutable, but ${SweetPattern.show(firstRefuttableArg.pattern)} is not`,
    }));
  }

  // we don't have to do anything if all the patterns are variables or _
  if (args.every(arg => arg.pattern.variant === 'Variable' || arg.pattern.variant === 'Any')) {
    return {
      args: args.map(({ pattern, mutable, annotation }, index) => ({
        name: NameEnv.declare(bodyEnv, pattern.variant === 'Variable' ? pattern.name : `_${index}`, mutable),
        mutable,
        annotation
      })),
      body: Expr.fromSweet(body, nameEnv, errors),
    };
  }

  // transform (p1, p2, ...) -> body into (arg1, arg2, ...) -> match { (p1, p2, ...) => body }
  // where arg1, arg2, ... are variables
  const varNames = args.map(({ mutable }, index) => NameEnv.declare(bodyEnv, `arg${index}`, mutable));
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