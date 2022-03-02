import { DataType, match as matchVariant, VariantOf } from "itsamatch";
import { Inst } from "../codegen/wasm/instructions";
import { Error } from "../errors/errors";
import { Impl } from "../infer/impls";
import { Tuple } from "../infer/tuples";
import { MonoTy, PolyTy, TypeParams } from "../infer/types";
import { Const } from "../parse/token";
import { Either } from "../utils/Either";
import { Maybe, none } from "../utils/maybe";
import { id } from "../utils/misc";
import { Name, NameEnv } from "./name";
import { Argument as SweetArgument, BinaryOperator, CompoundAssignmentOperator, Decl as SweetDecl, Expr as SweetExpr, MethodSig, Pattern as SweetPattern, Prog as SweetProg, Stmt as SweetStmt, UnaryOperator } from "./sweet";

// Bitter expressions are *unsugared* representations
// of the structure of yolang source code
// with attached type information and lexically resolved identifier references

type WithSweetRefAndType<T> = {
  [K in keyof T]: T[K] & { sweet: SweetExpr, ty: MonoTy }
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
  fromSweet: (sweet: SweetPattern, nameEnv: NameEnv): Pattern => matchVariant(sweet, {
    Const: ({ value }): Pattern => Pattern.Const(value),
    Variable: ({ name }): Pattern => Pattern.Variable(NameEnv.declare(nameEnv, name, false)),
    Tuple: ({ elements }): Pattern => Pattern.Tuple(elements.map(e => Pattern.fromSweet(e, nameEnv))),
    Any: (): Pattern => Pattern.Any(),
    Error: ({ message }): Pattern => Pattern.Error(message),
  }),
  type: (pattern: Pattern): MonoTy => matchVariant(pattern, {
    Const: ({ value }) => Const.type(value),
    Variable: ({ name }) => name.ty,
    Tuple: ({ elements }) => MonoTy.Tuple(Tuple.fromArray(elements.map(Pattern.type))),
    Any: (): MonoTy => MonoTy.fresh(),
    Error: (): MonoTy => MonoTy.fresh(),
  }),
  vars: (pattern: Pattern): Name[] => matchVariant(pattern, {
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
  Call: { lhs: Expr, args: Expr[] },
  BinaryOp: { lhs: Expr, op: BinaryOperator, rhs: Expr },
  UnaryOp: { op: UnaryOperator, expr: Expr },
  Error: { message: string },
  Closure: { args: Argument[], body: Expr },
  Block: { statements: Stmt[], lastExpr: Maybe<Expr> },
  IfThenElse: { condition: Expr, then: Expr, else_: Maybe<Expr> },
  Assignment: { lhs: Expr, rhs: Expr },
  MethodCall: { receiver: Expr, method: string, args: Expr[], impl: Maybe<Impl> },
  ModuleAccess: { path: string[], member: string },
  FieldAccess: { lhs: Expr, field: string },
  Tuple: { elements: Expr[] },
  Match: { expr: Expr, annotation: Maybe<MonoTy>, cases: { pattern: Pattern, annotation: Maybe<MonoTy>, body: Expr }[] },
  NamedRecord: { path: string[], name: string, typeParams: MonoTy[], fields: { name: string, value: Expr }[] },
  TupleIndexing: { lhs: Expr, index: number },
  WasmBlock: { instructions: Either<Inst, [Expr, Maybe<MonoTy>]>[] },
}>>;

const typed = <T extends {}>(obj: T, sweet: SweetExpr): T & { ty: MonoTy, sweet: SweetExpr } => ({
  ...obj,
  sweet,
  ty: MonoTy.fresh(),
});

export const Expr = {
  Const: (c: Const, sweet: SweetExpr): Expr => ({ variant: 'Const', value: c, sweet, ty: Const.type(c) }),
  Variable: (name: Name, sweet: SweetExpr): Expr => typed({ variant: 'Variable', name }, sweet),
  Call: (lhs: Expr, args: Expr[], sweet: SweetExpr): Expr => typed({ variant: 'Call', lhs, args }, sweet),
  BinaryOp: (lhs: Expr, op: BinaryOperator, rhs: Expr, sweet: SweetExpr): Expr => typed({ variant: 'BinaryOp', lhs, op, rhs }, sweet),
  UnaryOp: (op: UnaryOperator, expr: Expr, sweet: SweetExpr): Expr => typed({ variant: 'UnaryOp', op, expr }, sweet),
  Error: (message: string, sweet: SweetExpr): Expr => ({ variant: 'Error', message, sweet, ty: MonoTy.unit() }),
  Closure: (args: Argument[], body: Expr, sweet: SweetExpr): Expr => typed({ variant: 'Closure', args, body }, sweet),
  Block: (statements: Stmt[], lastExpr: Maybe<Expr>, sweet: SweetExpr): Expr => typed({ variant: 'Block', statements, lastExpr }, sweet),
  IfThenElse: (condition: Expr, then: Expr, else_: Maybe<Expr>, sweet: SweetExpr): Expr => typed({ variant: 'IfThenElse', condition, then, else_ }, sweet),
  Assignment: (lhs: Expr, rhs: Expr, sweet: SweetExpr): Expr => typed({ variant: 'Assignment', lhs, rhs }, sweet),
  MethodCall: (receiver: Expr, method: string, args: Expr[], sweet: SweetExpr): Expr => typed({ variant: 'MethodCall', receiver, method, args, impl: none }, sweet),
  ModuleAccess: (path: string[], member: string, sweet: SweetExpr): Expr => typed({ variant: 'ModuleAccess', path, member }, sweet),
  FieldAccess: (lhs: Expr, field: string, sweet: SweetExpr): Expr => typed({ variant: 'FieldAccess', lhs, field }, sweet),
  Tuple: (elements: Expr[], sweet: SweetExpr): Expr => typed({ variant: 'Tuple', elements }, sweet),
  Match: (expr: Expr, annotation: Maybe<MonoTy>, cases: { pattern: Pattern, annotation: Maybe<MonoTy>, body: Expr }[], sweet: SweetExpr): Expr => typed({ variant: 'Match', expr, annotation, cases }, sweet),
  NamedRecord: (path: string[], name: string, typeParams: MonoTy[], fields: { name: string, value: Expr }[], sweet: SweetExpr): Expr => typed({ variant: 'NamedRecord', path, name, typeParams, fields }, sweet),
  TupleIndexing: (lhs: Expr, index: number, sweet: SweetExpr): Expr => typed({ variant: 'TupleIndexing', lhs, index }, sweet),
  WasmBlock: (instructions: Either<Inst, [Expr, Maybe<MonoTy>]>[], sweet: SweetExpr): Expr => typed({ variant: 'WasmBlock', instructions }, sweet),
  fromSweet: (sweet: SweetExpr, nameEnv: NameEnv, errors: Error[]): Expr => {
    const go = (expr: SweetExpr, env = nameEnv) => Expr.fromSweet(expr, env, errors);

    return matchVariant(sweet, {
      Const: ({ value }) => Expr.Const(value, sweet),
      Variable: ({ name }) => Expr.Variable(NameEnv.resolve(nameEnv, name), sweet),
      Call: ({ lhs, args }) => Expr.Call(go(lhs), args.map(arg => go(arg)), sweet),
      BinaryOp: ({ lhs, op, rhs }) => Expr.BinaryOp(go(lhs), op, go(rhs), sweet),
      UnaryOp: ({ op, expr }) => Expr.UnaryOp(op, go(expr), sweet),
      Error: ({ message }) => Expr.Error(message, sweet),
      Closure: ({ args, body }) => {
        const withoutPatterns = removeFuncArgsPatternMatching(args, body, nameEnv, errors);
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
      MethodCall: ({ receiver, method, args }) => Expr.MethodCall(go(receiver), method, args.map(arg => go(arg)), sweet),
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
      WasmBlock: ({ instructions }) => Expr.WasmBlock(instructions.map(i => i.map({ left: id, right: ([expr, ty]) => [go(expr), ty] })), sweet)
    });
  },
  showSweet: (expr: Expr): string => SweetExpr.show(expr.sweet),
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
    return matchVariant(sweet, {
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
};

export type Decl = DataType<{
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
  Impl: { ty: MonoTy, typeParams: TypeParams, decls: Decl[] },
  TraitImpl: { trait: { path: string[], name: string, args: MonoTy[] }, typeParams: TypeParams, implementee: MonoTy, methods: Decl[] },
  Trait: { name: string, typeParams: TypeParams, methods: MethodSig[] },
  Error: { message: string },
}>;

export const Decl = {
  Function: (name: Name, typeParams: TypeParams, args: { name: Name, mutable: boolean, annotation: Maybe<MonoTy> }[], returnTy: Maybe<MonoTy>, body: Expr): Decl => ({ variant: 'Function', name, typeParams, args, body, returnTy, funTy: PolyTy.fresh() }),
  Module: (name: string, decls: Decl[]): Decl => {
    const mod: VariantOf<Decl, 'Module'> = {
      variant: 'Module',
      name,
      decls,
      members: {},
    };

    for (const decl of decls) {
      matchVariant(decl, {
        Function: func => {
          mod.members[func.name.original] = func;
        },
        Module: subMod => {
          mod.members[subMod.name] = subMod;
        },
        TypeAlias: alias => {
          mod.members[alias.name] = alias;
        },
        Impl: () => { },
        TraitImpl: () => { },
        Trait: trait => {
          mod.members[trait.name] = trait;
        },
        Error: () => { },
      });
    }

    return mod;
  },
  TypeAlias: (name: string, typeParams: TypeParams, alias: MonoTy): Decl => ({ variant: 'TypeAlias', name, typeParams, alias }),
  Impl: (ty: MonoTy, typeParams: TypeParams, decls: Decl[]): Decl => ({ variant: 'Impl', ty, typeParams, decls }),
  TraitImpl: (trait: { path: string[], name: string, args: MonoTy[] }, typeParams: TypeParams, implementee: MonoTy, methods: Decl[]): Decl => ({ variant: 'TraitImpl', trait, typeParams, implementee, methods }),
  Trait: (name: string, typeParams: TypeParams, methods: MethodSig[]): Decl => ({ variant: 'Trait', name, typeParams, methods }),
  Error: (message: string): Decl => ({ variant: 'Error', message }),
  fromSweet: (sweet: SweetDecl, nameEnv: NameEnv, declareFuncNames: boolean, moduleStack: string[], errors: Error[]): Decl =>
    matchVariant(sweet, {
      Function: ({ name, typeParams, args, returnTy, body }) => {
        const withoutPatterns = removeFuncArgsPatternMatching(args, body, nameEnv, errors);
        const nameRef = declareFuncNames ? NameEnv.declare(nameEnv, name, false) : NameEnv.resolve(nameEnv, name);
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
      InherentImpl: ({ ty, typeParams, decls }) => {
        const implEnv = NameEnv.clone(nameEnv);

        for (const decl of decls) {
          if (decl.variant === 'Function') {
            NameEnv.declare(implEnv, decl.name, false);
          }
        }

        return Decl.Impl(
          ty,
          typeParams,
          decls.map(decl => Decl.fromSweet(decl, implEnv, false, [...moduleStack, MonoTy.show(ty)], errors))
        );
      },
      TraitImpl: ({ trait, typeParams, implementee, methods }) => {
        const implEnv = NameEnv.clone(nameEnv);

        for (const method of methods) {
          if (method.variant === 'Function') {
            NameEnv.declare(implEnv, method.name, false);
          }
        }

        return Decl.TraitImpl(
          trait,
          typeParams,
          implementee,
          methods.map(method => Decl.fromSweet(method, implEnv, false, [...moduleStack, trait.name, MonoTy.show(implementee)], errors))
        );
      },
      Trait: ({ name, typeParams, methods }) => Decl.Trait(name, typeParams, methods),
      Error: ({ message }) => Decl.Error(message),
    }),
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
};

export const removeFuncArgsPatternMatching = (
  args: SweetArgument[],
  body: SweetExpr,
  nameEnv: NameEnv,
  errors: Error[],
): { args: Argument[], body: Expr } => {
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
        name: NameEnv.declare(nameEnv, pattern.variant === 'Variable' ? pattern.name : `_${index}`, mutable),
        mutable,
        annotation
      })),
      body: Expr.fromSweet(body, nameEnv, errors),
    };
  }

  // transform (p1, p2, ...) -> body into (arg1, arg2, ...) -> match { (p1, p2, ...) => body }
  // where arg1, arg2, ... are variables
  const bodyEnv = NameEnv.make();
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