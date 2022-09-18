import { DataType, genConstructors, match, VariantOf } from "itsamatch";
import { Error } from "../errors/errors";
import { Tuple } from "../infer/tuples";
import { TypeContext } from "../infer/typeContext";
import { MonoTy, PolyTy, TypeParam, TypeParams, TyVar } from "../infer/types";
import { Const } from "../parse/token";
import { joinWith, last, zip } from "../utils/array";
import { Either } from "../utils/either";
import { Maybe, none, some } from "../utils/maybe";
import { assert, id, mapMap, proj, pushMap } from "../utils/misc";
import { Context } from "./context";
import { FuncName, NameEnv, VarName } from "./name";
import * as sweet from "./sweet";

// Bitter expressions are *unsugared* representations
// of the structure of yolang source code
// with attached type information and lexically resolved identifier references

type WithSweetRefAndType<T> = {
  [K in keyof T]: T[K] & { sweet: sweet.Expr, ty: MonoTy }
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
  from: (sweet: sweet.Pattern, nameEnv: NameEnv): Pattern => match(sweet, {
    Const: ({ value }): Pattern => Pattern.Const(value),
    Variable: ({ name }): Pattern => Pattern.Variable(NameEnv.declareVar(nameEnv, name, false)),
    Tuple: ({ elements }): Pattern => Pattern.Tuple(elements.map(e => Pattern.from(e, nameEnv))),
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
  show: (pattern: Pattern): string => match(pattern, {
    Const: ({ value }) => `${Const.show(value)}`,
    Variable: ({ name }) => `${name}`,
    Tuple: ({ elements }) => `(${joinWith(elements, Pattern.show)})`,
    Any: () => '_',
    Error: ({ message }) => `<Error: ${message}>`,
  }),
};

type Argument = { name: VarName, mutable: boolean, annotation: Maybe<MonoTy> };

export const Argument = {
  asMonoTy: ({ annotation }: Argument): MonoTy => annotation.orDefault(MonoTy.fresh),
};

export type ArrayInit = DataType<{
  elems: { elems: Expr[] },
  fill: { value: Expr, count: number },
}>;

export const ArrayInit = {
  ...genConstructors<ArrayInit>(['elems', 'fill']),
  show: (init: ArrayInit): string => match(init, {
    elems: ({ elems }) => `[${elems.map(Expr.show).join(', ')}]`,
    fill: ({ value, count }) => `[${Expr.show(value)}; ${count}]`,
  }),
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
  FieldAccess: { lhs: Expr, field: string },
  Array: { init: ArrayInit, elemTy: MonoTy },
  Tuple: { elements: Expr[] },
  Match: { expr: Expr, annotation: Maybe<MonoTy>, cases: { pattern: Pattern, annotation: Maybe<MonoTy>, body: Expr }[] },
  Struct: { name: string, typeParams: MonoTy[], fields: { name: string, value: Expr }[] },
  TupleIndexing: { lhs: Expr, index: number },
}>>;

const typed = <T extends {}>(obj: T, sweet: sweet.Expr): T & { ty: MonoTy, sweet: sweet.Expr } => ({
  ...obj,
  sweet,
  ty: MonoTy.fresh(),
});

export const Expr = {
  Const: (c: Const, sweet: sweet.Expr): Expr => ({ variant: 'Const', value: c, sweet, ty: Const.type(c) }),
  Variable: (name: VarName, sweet: sweet.Expr): Expr => typed({ variant: 'Variable', name }, sweet),
  NamedFuncCall: (name: Either<string, FuncName>, typeParams: MonoTy[], args: Expr[], sweet: sweet.Expr): Expr => typed({ variant: 'NamedFuncCall', name, typeParams, args }, sweet),
  Call: (lhs: Expr, args: Expr[], sweet: sweet.Expr): Expr => typed({ variant: 'Call', lhs, args }, sweet),
  Error: (message: string, sweet: sweet.Expr): Expr => ({ variant: 'Error', message, sweet, ty: MonoTy.void() }),
  Closure: (args: Argument[], body: Expr, sweet: sweet.Expr): Expr => typed({ variant: 'Closure', args, body }, sweet),
  Block: (statements: Stmt[], lastExpr: Maybe<Expr>, sweet: sweet.Expr): Expr => typed({ variant: 'Block', statements, lastExpr }, sweet),
  IfThenElse: (condition: Expr, then: Expr, else_: Maybe<Expr>, sweet: sweet.Expr): Expr => typed({ variant: 'IfThenElse', condition, then, else_ }, sweet),
  FieldAccess: (lhs: Expr, field: string, sweet: sweet.Expr): Expr => typed({ variant: 'FieldAccess', lhs, field }, sweet),
  Array: (init: ArrayInit, sweet: sweet.Expr, elemTy = MonoTy.fresh()): Expr => ({ variant: 'Array', init, elemTy, ty: MonoTy.Array(elemTy), sweet }),
  Tuple: (elements: Expr[], sweet: sweet.Expr): Expr => typed({ variant: 'Tuple', elements }, sweet),
  Match: (expr: Expr, annotation: Maybe<MonoTy>, cases: { pattern: Pattern, annotation: Maybe<MonoTy>, body: Expr }[], sweet: sweet.Expr): Expr => typed({ variant: 'Match', expr, annotation, cases }, sweet),
  Struct: (name: string, typeParams: MonoTy[], fields: { name: string, value: Expr }[], sweet: sweet.Expr): Expr => typed({ variant: 'Struct', name, typeParams, fields }, sweet),
  TupleIndexing: (lhs: Expr, index: number, sweet: sweet.Expr): Expr => typed({ variant: 'TupleIndexing', lhs, index }, sweet),
  from: (sweet: sweet.Expr, nameEnv: NameEnv, errors: Error[]): Expr => {
    const go = (expr: sweet.Expr, env = nameEnv) => Expr.from(expr, env, errors);
    const resolveTy = (ty: MonoTy) => NameEnv.resolveType(nameEnv, ty);

    return match(sweet, {
      Const: ({ value }) => Expr.Const(value, sweet),
      Variable: ({ name }) => Expr.Variable(NameEnv.resolveVar(nameEnv, name), sweet),
      Call: ({ lhs, typeParams, args }) => match(lhs, {
        Variable: ({ name }) => Expr.NamedFuncCall(
          Either.left(name),
          typeParams.map(resolveTy),
          args.map(arg => go(arg)),
          sweet
        ),
        _: () => Expr.Call(go(lhs), args.map(arg => go(arg)), sweet),
      }),
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
        return Expr.Block(statements.map(s => Stmt.from(s, newNameEnv, errors)), lastExpr.map(e => go(e, newNameEnv)), sweet);
      },
      IfThenElse: ite => {
        const rewriteElseIfs = (
          cond: sweet.Expr,
          then: sweet.Expr,
          elifs: { cond: sweet.Expr, body: sweet.Expr }[],
          else_: Maybe<sweet.Expr>,
        ): Expr => {
          if (elifs.length === 0) {
            return Expr.IfThenElse(
              go(cond),
              go(then),
              else_.map(go),
              sweet
            );
          }

          const [head, ...tail] = elifs;

          return Expr.IfThenElse(
            go(cond),
            go(then),
            some(rewriteElseIfs(
              head.cond,
              head.body,
              tail,
              else_,
            )),
            sweet,
          );
        };

        return rewriteElseIfs(ite.condition, ite.then, ite.elseifs, ite.else_);
      },
      FieldAccess: ({ lhs, field }) => Expr.FieldAccess(go(lhs), field, sweet),
      Array: ({ init }) => Expr.Array(match(init, {
        elems: ({ elems }) => ArrayInit.elems({ elems: elems.map(e => go(e)) }),
        fill: ({ value, count }) => ArrayInit.fill({ count, value: go(value) }),
      }), sweet),
      Tuple: ({ elements }) => Expr.Tuple(elements.map(e => go(e)), sweet),
      Match: ({ expr, annotation, cases }) => Expr.Match(
        go(expr),
        annotation.map(resolveTy),
        cases.map(c => {
          const bodyEnv = NameEnv.clone(nameEnv);
          return {
            pattern: Pattern.from(c.pattern, bodyEnv),
            annotation: c.annotation.map(resolveTy),
            body: go(c.body, bodyEnv)
          };
        }),
        sweet
      ),
      Parenthesized: ({ expr }) => go(expr),
      Struct: ({ name, typeParams, fields }) => Expr.Struct(
        name,
        typeParams.map(resolveTy),
        fields.map(f => ({ name: f.name, value: go(f.value) })),
        sweet
      ),
      TupleIndexing: ({ lhs, index }) => Expr.TupleIndexing(go(lhs), index, sweet),
      LetIn: ({ pattern, annotation, value, body }) => {
        // let pat = v in b --> match v with { pat => body }
        return Expr.Match(
          go(value),
          none,
          [{ pattern: Pattern.from(pattern, nameEnv), annotation: annotation.map(resolveTy), body: go(body) }],
          sweet
        );
      },
    });
  },
  showSweet: (expr: Expr): string => sweet.Expr.show(expr.sweet),
  show: (expr: Expr): string => match(expr, {
    Const: ({ value: expr }) => Const.show(expr),
    Variable: ({ name }) => VarName.show(name),
    NamedFuncCall: ({ name, typeParams, args }) => {
      const funcName: string = name.match({ left: id, right: FuncName.show });
      const params = `${typeParams.length > 0 ? `<${joinWith(typeParams, MonoTy.show)}>` : ''}`;
      return `${funcName}${params}(${joinWith(args, Expr.show, ', ')})`;
    },
    Error: ({ message }) => `<Error: ${message}>`,
    Block: ({ statements, lastExpr }) => `{\n${joinWith([...statements, ...lastExpr.mapWithDefault(e => [Stmt.Expr(e)], [])], s => '  ' + Stmt.show(s), '\n')}\n}`,
    IfThenElse: ({ condition, then, else_ }) => `if ${Expr.show(condition)} ${Expr.show(then)}${else_.map(e => ` else ${Expr.show(e)}`).orDefault('')}`,
    FieldAccess: ({ lhs, field }) => `${Expr.show(lhs)}.${field}`,
    Tuple: ({ elements }) => `(${joinWith(elements, Expr.show, ', ')})`,
    Array: ({ init }) => ArrayInit.show(init),
    Match: ({ expr, cases }) => `match ${Expr.show(expr)} {\n${joinWith(cases, ({ pattern, body }) => `  ${Pattern.show(pattern)} => ${Expr.show(body)}\n`, '\n')}\n}`,
    Struct: ({ name, typeParams, fields }) => {
      const tyParamsFmt = typeParams.length > 0 ? `<${joinWith(typeParams, MonoTy.show, ', ')}>` : '';
      const inline = fields.length <= 2;
      let fieldsFmt = joinWith(fields, ({ name, value }) => `${name}: ${Expr.show(value)}`, inline ? ', ' : ',\n');
      fieldsFmt = inline ? `{ ${fieldsFmt} }` : `{\n${fieldsFmt}\n}`;
      return `${name}${tyParamsFmt} ${fieldsFmt}`;
    },
    _: () => Expr.showSweet(expr),
  }),
  rewrite: (expr: Expr, nameEnv: NameEnv, rewriteExpr: (expr: Expr) => Expr, rewriteTy: (ty: MonoTy) => MonoTy): Expr => {
    const go = (expr: Expr): Expr => Expr.rewrite(expr, nameEnv, rewriteExpr, rewriteTy);

    return rewriteExpr(match(expr, {
      Const: ({ value }) => Expr.Const(value, expr.sweet),
      Variable: ({ name }) => Expr.Variable(NameEnv.resolveVar(nameEnv, name.original), expr.sweet),
      NamedFuncCall: ({ name, typeParams, args }) => Expr.NamedFuncCall(name, typeParams.map(rewriteTy), args.map(arg => go(arg)), expr.sweet),
      Call: ({ lhs, args }) => Expr.Call(go(lhs), args.map(arg => go(arg)), expr.sweet),
      Error: ({ message }) => Expr.Error(message, expr.sweet),
      Closure: ({ args, body }) => Expr.Closure(
        args.map(arg => ({
          mutable: arg.mutable,
          name: NameEnv.resolveVar(nameEnv, arg.name.original),
          annotation: arg.annotation.map(rewriteTy)
        })),
        go(body),
        expr.sweet
      ),
      Block: ({ statements, lastExpr }) => Expr.Block(statements.map(s => Stmt.rewrite(s, nameEnv, rewriteExpr, rewriteTy)), lastExpr.map(e => go(e)), expr.sweet),
      IfThenElse: ({ condition, then, else_ }) => Expr.IfThenElse(go(condition), go(then), else_.map(go), expr.sweet),
      FieldAccess: ({ lhs, field }) => Expr.FieldAccess(go(lhs), field, expr.sweet),
      Array: ({ init, elemTy }) => Expr.Array(match(init, {
        elems: ({ elems }) => ArrayInit.elems({ elems: elems.map(e => go(e)) }),
        fill: ({ value, count }) => ArrayInit.fill({ count, value: go(value) }),
      }), expr.sweet, rewriteTy(elemTy)),
      Tuple: ({ elements }) => Expr.Tuple(elements.map(e => go(e)), expr.sweet),
      Match: ({ expr, annotation, cases }) => Expr.Match(go(expr), annotation.map(rewriteTy), cases.map(c => ({ ...c, body: go(c.body) })), expr.sweet),
      Struct: ({ name, typeParams, fields }) => Expr.Struct(name, typeParams.map(rewriteTy), fields.map(f => ({ ...f, value: go(f.value) })), expr.sweet),
      TupleIndexing: ({ lhs, index }) => Expr.TupleIndexing(go(lhs), index, expr.sweet),
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
  Assignment: { lhs: Expr, rhs: Expr },
  Expr: { expr: Expr },
  While: { condition: Expr, statements: Stmt[] },
  Return: { expr: Maybe<Expr> },
  Error: { message: string },
}>;

export const Stmt = {
  Let: (name: VarName, expr: Expr, mutable: boolean, annotation: Maybe<MonoTy>): Stmt => ({ variant: 'Let', name, expr, mutable, annotation }),
  Assignment: (lhs: Expr, rhs: Expr): Stmt => ({ variant: 'Assignment', lhs, rhs }),
  Expr: (expr: Expr): Stmt => ({ variant: 'Expr', expr }),
  While: (condition: Expr, statements: Stmt[]): Stmt => ({ variant: 'While', condition, statements }),
  Return: (expr: Maybe<Expr>): Stmt => ({ variant: 'Return', expr }),
  Error: (message: string): Stmt => ({ variant: 'Error', message }),
  from: (sweet: sweet.Stmt, nameEnv: NameEnv, errors: Error[]): Stmt => match(sweet, {
    Let: ({ name, expr, mutable, annotation }) => Stmt.Let(
      NameEnv.declareVar(nameEnv, name, mutable),
      Expr.from(expr, nameEnv, errors),
      mutable,
      annotation.map(ann => NameEnv.resolveType(nameEnv, ann))
    ),
    Assignment: ({ lhs, rhs }) => Stmt.Assignment(Expr.from(lhs, nameEnv, errors), Expr.from(rhs, nameEnv, errors)),
    Expr: ({ expr }) => Stmt.Expr(Expr.from(expr, nameEnv, errors)),
    While: ({ condition, statements }) => Stmt.While(Expr.from(condition, nameEnv, errors), statements.map(s => Stmt.from(s, nameEnv, errors))),
    Return: ({ expr }) => Stmt.Return(expr.map(expr => Expr.from(expr, nameEnv, errors))),
    Error: ({ message }) => Stmt.Error(message),
  }),
  show: (stmt: Stmt): string => match(stmt, {
    Let: ({ name, expr, mutable, annotation }) => `${mutable ? 'mut' : 'let'} ${VarName.show(name)}${annotation.mapWithDefault(ty => ': ' + MonoTy.show(ty), '')} = ${Expr.show(expr)}`,
    Assignment: ({ lhs, rhs }) => `${Expr.show(lhs)} = ${Expr.show(rhs)}`,
    Expr: ({ expr }) => Expr.show(expr),
    While: ({ condition, statements }) => `while ${Expr.show(condition)} {\n${statements.map(Stmt.show).join('\n')}\n}`,
    Return: ({ expr }) => `return ${expr.mapWithDefault(Expr.show, '')}`,
    Error: ({ message }) => `<Error: ${message}>`,
  }),
  rewrite: (stmt: Stmt, nameEnv: NameEnv, rewriteExpr: (expr: Expr) => Expr, rewriteTy: (ty: MonoTy) => MonoTy): Stmt => {
    const goExpr = (expr: Expr) => Expr.rewrite(expr, nameEnv, rewriteExpr, rewriteTy);
    return match(stmt, {
      Let: ({ name, expr, mutable, annotation }) => Stmt.Let(
        NameEnv.resolveVar(nameEnv, name.original),
        goExpr(expr),
        mutable,
        annotation.map(rewriteTy)
      ),
      Assignment: ({ lhs, rhs }) => Stmt.Assignment(goExpr(lhs), goExpr(rhs)),
      Expr: ({ expr }) => Stmt.Expr(goExpr(expr)),
      While: ({ condition, statements }) => Stmt.While(
        goExpr(condition),
        statements.map(s => Stmt.rewrite(s, nameEnv, rewriteExpr, rewriteTy))
      ),
      Return: ({ expr }) => Stmt.Return(expr.map(goExpr)),
      Error: ({ message }) => Stmt.Error(message),
    });
  },
};

type FuncArg = { mutable: boolean, name: VarName, annotation: Maybe<MonoTy> };

const FuncArg = {
  show: ({ mutable, name, annotation }: FuncArg): string => {
    const base = `${mutable ? 'mut ' : ''}${name.original}`;
    const ann = annotation.mapWithDefault(ty => ': ' + MonoTy.show(ty), '');
    return `${base}${ann}`;
  },
};

export type Decl = DataType<{
  Function: {
    attributes: sweet.Attribute[],
    pub: boolean,
    name: FuncName,
    typeParams: { name: string, ty: Maybe<MonoTy> }[],
    args: FuncArg[],
    returnTy: Maybe<MonoTy>,
    body: Maybe<Expr>,
    funTy: PolyTy,
    instances: Map<string, MonoTy[]>,
  },
  TypeAlias: {
    pub: boolean,
    name: string,
    typeParams: TypeParam[],
    alias: MonoTy,
  },
  Error: { message: string },
}>;

const { TypeAlias } = genConstructors<Decl>(['TypeAlias']);

type FuncConstructorParams = {
  attributes: sweet.Attribute[],
  pub: boolean,
  name: FuncName,
  typeParams: { name: string, ty: Maybe<MonoTy> }[],
  args: FuncArg[],
  returnTy: Maybe<MonoTy>,
  body: Maybe<Expr>,
};

export const Decl = {
  Function: ({ attributes, pub, name, typeParams, args, returnTy, body }: FuncConstructorParams): Decl => {
    const quantifiedVars = typeParams.map(Context.freshTyVarIndex);
    const newParams = zip(typeParams, quantifiedVars).map(([{ name }, id]) => ({ name, ty: some(MonoTy.Var(TyVar.Unbound(id))) }));
    const subst = new Map<string, MonoTy>(newParams.map(({ name, ty }) => [name, ty.unwrap()]));
    const funTy = PolyTy.make(quantifiedVars, MonoTy.substituteTyParams(MonoTy.Fun(
      args.map(a => a.annotation.orDefault(MonoTy.fresh)),
      returnTy.orDefault(body.mapWithDefault(proj('ty'), MonoTy.fresh))
    ), subst));

    return {
      variant: 'Function',
      attributes,
      pub,
      name,
      typeParams: newParams,
      args,
      body: body.map(body => {
        assert(body.variant === 'Block');
        if (body.statements.length > 0) {
          // if the last statement is a return statement
          // then put it in the lastExpr field instead
          const lastStmt = last(body.statements);
          if (body.lastExpr.isNone() && lastStmt.variant === 'Return') {
            return Expr.Block(body.statements.slice(0, -1), lastStmt.expr, body.sweet);
          }
        }

        return body;
      }),
      returnTy,
      funTy,
      instances: new Map(),
    };
  },
  TypeAlias,
  Error: (message: string): Decl => ({ variant: 'Error', message }),
  from: (decl: sweet.Decl, nameEnv: NameEnv, errors: Error[]): Decl[] =>
    match(decl, {
      Function: ({ attributes, pub, name, typeParams, args, returnTy, body }) => {
        const nameRef = NameEnv.declareFunc(nameEnv, name);
        typeParams.forEach(({ name, ty }) => {
          NameEnv.declareTypeParam(nameEnv, name, ty.orDefault(() => MonoTy.Param(name)));
        });

        const withoutPatterns = rewriteFuncArgsPatternMatching(args, body.orDefault(sweet.Expr.Block([], none)), nameEnv, errors);

        return [Decl.Function({
          attributes,
          pub,
          name: nameRef,
          typeParams,
          args: withoutPatterns.args,
          returnTy: returnTy.map(ty => NameEnv.resolveType(nameEnv, ty)),
          body: body.match({
            Some: () => some(withoutPatterns.body),
            None: () => none,
          }),
        })];
      },
      TypeAlias: ({ pub, name, typeParams, alias }) => {
        typeParams.forEach(({ name, ty }) => {
          NameEnv.declareTypeParam(nameEnv, name, ty.orDefault(() => MonoTy.Param(name)));
        });

        return [Decl.TypeAlias({ pub, name, typeParams, alias: NameEnv.resolveType(nameEnv, alias) })];
      },
      Import: () => [],
      Error: ({ message }) => [Decl.Error(message)],
    }),
  show: (decl: Decl): string => match(decl, {
    Function: ({ attributes, name, typeParams, args, returnTy, body }) => {
      const attrsFmt = attributes.length > 0 ? sweet.Attribute.showMany(attributes) + '\n' : '';
      const argsFmt = `${TypeParams.show(typeParams)}(${joinWith(args, FuncArg.show, ', ')})`;
      const retTyFmt = returnTy.mapWithDefault(ty => ': ' + MonoTy.show(ty), '');
      const bodyFmt = body.mapWithDefault(Expr.show, '');
      return `${attrsFmt}fun ${FuncName.show(name)}${argsFmt}${retTyFmt} ${bodyFmt}`;
    },
    TypeAlias: ({ name, typeParams, alias }) => `type ${name}${TypeParams.show(typeParams)} = ${MonoTy.show(alias)} `,
    Error: ({ message }) => `< Error: ${message}> `,
  }),
  rewrite: (decl: Decl, nameEnv: NameEnv, rewriteExpr: (expr: Expr) => Expr, rewriteTy: (ty: MonoTy) => MonoTy = id): Decl => match(decl, {
    Function: ({ attributes, pub, name, typeParams, args, body, returnTy }) => {
      const bodyEnv = NameEnv.clone(nameEnv);
      return Decl.Function({
        attributes,
        pub,
        name: NameEnv.declareFunc(nameEnv, name.original, name.mangled),
        typeParams: typeParams.map(p => ({ name: p.name, ty: p.ty.map(rewriteTy) })),
        args: args.map(arg => ({
          mutable: arg.mutable,
          annotation: arg.annotation.map(rewriteTy),
          name: NameEnv.declareVar(bodyEnv, arg.name.original, arg.name.mutable, arg.name.mangled)
        })),
        returnTy: returnTy.map(rewriteTy),
        body: body.map(b => Expr.rewrite(b, bodyEnv, rewriteExpr, rewriteTy)),
      });
    },
    TypeAlias: ({ pub, name, typeParams, alias }) => Decl.TypeAlias({
      pub,
      name,
      typeParams: typeParams.map(p => ({ name: p.name, ty: p.ty.map(rewriteTy) })),
      alias: rewriteTy(alias),
    }),
    Error: ({ message }) => Decl.Error(message),
  }),
};

export type BitterConversionError = {
  message: string,
};

export type Module = {
  name: string,
  path: string,
  decls: Decl[],
  members: Map<string, VariantOf<Decl, 'Function' | 'TypeAlias'>[]>,
  imports: Map<string, { sourceMod: string, isExport: boolean }>,
  typeContext: TypeContext,
  typeChecked: boolean,
};

const Module = {
  from: (mod: sweet.Module, modules: Map<string, Module>, nameEnv: NameEnv, errors: Error[]): Module => {
    const decls = mod.decls.flatMap(d => Decl.from(d, nameEnv, errors));
    const bitterMod: Module = {
      name: mod.name,
      path: mod.path,
      decls,
      imports: mod.imports,
      members: new Map(),
      typeContext: TypeContext.make(modules),
      typeChecked: false,
    };

    for (const decl of decls) {
      match(decl, {
        Function: f => {
          pushMap(bitterMod.members, f.name.original, f);
        },
        TypeAlias: t => {
          pushMap(bitterMod.members, t.name, t);
        },
        _: () => { },
      });
    }

    return bitterMod;
  },
  shallowClone: (mod: Module): Module => ({
    name: mod.name,
    path: mod.path,
    decls: [...mod.decls],
    members: new Map(mod.members),
    imports: mapMap(mod.imports, s => ({ ...s })),
    typeContext: TypeContext.clone(mod.typeContext),
    typeChecked: mod.typeChecked,
  }),
};

export type Prog = {
  modules: Map<string, Module>,
  nameEnv: NameEnv,
  entry: Module,
};

export const Prog = {
  from: (prog: sweet.Prog): [prog: Prog, errors: Error[]] => {
    const nameEnv = NameEnv.make();
    const errors: Error[] = [];
    const bitterModules = new Map<string, Module>();

    for (const [path, mod] of prog.modules) {
      bitterModules.set(path, Module.from(mod, bitterModules, nameEnv, errors));
    }

    const bitterProg: Prog = {
      modules: bitterModules,
      nameEnv,
      entry: bitterModules.get(prog.entry.path)!,
    };

    return [bitterProg, errors];
  },
  shallowClone: (prog: Prog): Prog => {
    const mods = mapMap(prog.modules, Module.shallowClone);

    return {
      modules: mods,
      entry: mods.get(prog.entry.path)!,
      nameEnv: NameEnv.clone(prog.nameEnv),
    };
  },
  show: (prog: Prog): string => joinWith(prog.entry.decls, Decl.show, '\n\n'),
};

const rewriteFuncArgsPatternMatching = (
  args: sweet.Argument[],
  body: sweet.Expr,
  nameEnv: NameEnv,
  errors: Error[],
): { args: Argument[], body: Expr } => {
  const bodyEnv = NameEnv.clone(nameEnv);
  const firstRefuttableArg = args.find(({ pattern }) => !sweet.Pattern.isIrrefutable(pattern));
  if (firstRefuttableArg !== undefined) {
    errors.push(Error.BitterConversion({
      message: `patterns in function arguments must be irrefutable, but ${sweet.Pattern.show(firstRefuttableArg.pattern)} is not`,
    }));
  }

  // we don't have to do anything if all the patterns are variables or _
  if (args.every(arg => arg.pattern.variant === 'Variable' || arg.pattern.variant === 'Any')) {
    const declaredArgs = args.map(({ pattern, mutable, annotation }, index) => ({
      name: NameEnv.declareVar(bodyEnv, pattern.variant === 'Variable' ? pattern.name : `_${index}`, mutable),
      mutable,
      annotation: annotation.map(ann => NameEnv.resolveType(nameEnv, ann))
    }));

    return {
      args: declaredArgs,
      body: Expr.from(body, bodyEnv, errors),
    };
  }

  // transform (p1, p2, ...) -> body into (arg1, arg2, ...) -> match { (p1, p2, ...) => body }
  // where arg1, arg2, ... are variables
  const varNames = args.map(({ mutable }, index) => NameEnv.declareVar(bodyEnv, `arg${index}`, mutable));
  const matchedExpr = args.length === 0 ?
    sweet.Expr.Variable(varNames[0].original) :
    sweet.Expr.Tuple(varNames.map(name => sweet.Expr.Variable(name.original)));

  const pattern = args.length === 0 ?
    args[0].pattern :
    sweet.Pattern.Tuple(args.map(({ pattern }) => pattern));


  const newBody = Expr.from(
    sweet.Expr.Match(matchedExpr, none, [{ pattern, annotation: none, body }]),
    bodyEnv,
    errors
  );

  return {
    args: varNames.map(arg => ({ name: arg, mutable: arg.mutable, annotation: none })),
    body: newBody,
  };
};