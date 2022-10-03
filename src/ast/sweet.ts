import { DataType, genConstructors, match, VariantOf } from 'itsamatch';
import { MonoTy, TypeParam, TypeParams } from '../infer/types';
import { Const, operators, Position, WithPos } from '../parse/token';
import { joinWith, last } from '../utils/array';
import { Maybe, none } from '../utils/maybe';
import { parenthesized } from '../utils/misc';

// Sweet expressions are *sugared* representations
// of the structure of yolang source code.

export type Argument = { pattern: Pattern, mutable: boolean, annotation: Maybe<MonoTy> };

export const Argument = {
  show: ({ pattern, mutable, annotation }: Argument): string => {
    return `${mutable ? 'mut ' : ''}${Pattern.show(pattern)}${annotation.mapWithDefault(t => `: ${MonoTy.show(t)}`, '')}`;
  },
  asMonoTy: ({ annotation }: Argument): MonoTy => {
    return annotation.orDefault(MonoTy.fresh);
  },
};

export const ArgumentList = {
  show: (args: Argument[]) => parenthesized(
    joinWith(args, Argument.show, ', '),
    args.length !== 1 || true || (
      args[0].mutable ||
      args[0].annotation.isSome() ||
      args[0].pattern.variant === 'Tuple'
    ),
  ),
};

export type Attribute = { name: string, args: string[] };

export const Attribute = {
  make: (name: string, args: string[]): Attribute => ({ name, args }),
  show: ({ name, args }: Attribute): string => {
    if (args.length === 0) {
      return name;
    }

    return `${name}(${args.join(', ')})`;
  },
  showMany: (attrs: Attribute[], type: 'inner' | 'outer'): string => {
    return (type === 'inner' ? '#!' : '#') + `[${attrs.map(Attribute.show).join(', ')}]`;
  },
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

export type Expr = DataType<WithPos<{
  Const: { value: Const },
  Variable: { name: string },
  Call: { lhs: Expr, typeParams: MonoTy[], args: Expr[] },
  Error: { message: string },
  Closure: { args: Argument[], body: Expr },
  Block: { statements: Stmt[], lastExpr: Maybe<Expr> },
  IfThenElse: { condition: Expr, then: Expr, elseifs: { cond: Expr, body: Expr }[], else_: Maybe<Expr> },
  FieldAccess: { lhs: Expr, field: string },
  Tuple: { elements: Expr[] },
  Array: { init: ArrayInit },
  Match: { expr: Expr, annotation: Maybe<MonoTy>, cases: { pattern: Pattern, annotation: Maybe<MonoTy>, body: Expr }[] },
  Parenthesized: { expr: Expr },
  Struct: { name: string, typeParams: MonoTy[], fields: { name: string, value: Expr }[] },
  TupleIndexing: { lhs: Expr, index: number },
  LetIn: { pattern: Pattern, annotation: Maybe<MonoTy>, value: Expr, body: Expr },
}>>;

export const Expr = {
  Const: (c: Const, pos: Position = Position.DONT_CARE): Expr => ({ variant: 'Const', value: c, pos }),
  Variable: (name: string, pos: Position = Position.DONT_CARE): Expr => ({ variant: 'Variable', name, pos }),
  Call: (lhs: Expr, typeParams: MonoTy[], args: Expr[], pos: Position = Position.DONT_CARE): Expr => ({ variant: 'Call', lhs, typeParams, args, pos }),
  Error: (message: string, pos: Position = Position.DONT_CARE): Expr => ({ variant: 'Error', message, pos }),
  Closure: (args: Argument[], body: Expr, pos: Position = Position.DONT_CARE): Expr => ({ variant: 'Closure', args, body, pos }),
  Block: (statements: Stmt[], lastExpr?: Maybe<Expr>, pos: Position = Position.DONT_CARE): Expr => ({ variant: 'Block', statements, lastExpr: lastExpr ?? none, pos }),
  IfThenElse: (condition: Expr, then: Expr, elseifs: { cond: Expr, body: Expr }[], else_: Maybe<Expr>, pos: Position): Expr => ({ variant: 'IfThenElse', condition, then, elseifs, else_, pos }),
  FieldAccess: (lhs: Expr, field: string, pos: Position = Position.DONT_CARE): Expr => ({ variant: 'FieldAccess', lhs, field, pos }),
  Tuple: (elements: Expr[], pos: Position = Position.DONT_CARE): Expr => ({ variant: 'Tuple', elements, pos }),
  Array: (init: ArrayInit, pos: Position = Position.DONT_CARE): Expr => ({ variant: 'Array', init, pos }),
  Match: (expr: Expr, annotation: Maybe<MonoTy>, cases: { pattern: Pattern, annotation: Maybe<MonoTy>, body: Expr }[], pos: Position = Position.DONT_CARE): Expr => ({ variant: 'Match', annotation, expr, cases, pos }),
  Parenthesized: (expr: Expr, pos: Position = Position.DONT_CARE): Expr => ({ variant: 'Parenthesized', expr, pos }),
  Struct: (name: string, typeParams: MonoTy[], fields: { name: string, value: Expr }[], pos: Position = Position.DONT_CARE): Expr => ({ variant: 'Struct', name, typeParams, fields, pos }),
  TupleIndexing: (lhs: Expr, index: number, pos: Position = Position.DONT_CARE): Expr => ({ variant: 'TupleIndexing', lhs, index, pos }),
  LetIn: (pattern: Pattern, annotation: Maybe<MonoTy>, value: Expr, body: Expr, pos: Position = Position.DONT_CARE): Expr => ({ variant: 'LetIn', pattern, annotation, value, body, pos }),
  generated: Object.freeze<Expr>({ variant: 'Const', value: Const.str('<generated>'), pos: Position.DONT_CARE }),
  show: (expr: Expr): string => match(expr, {
    Const: ({ value: expr }) => Const.show(expr),
    Variable: ({ name }) => name,
    Call: ({ lhs, typeParams, args }) => {
      if (lhs.variant === 'Variable' && operators.has(lhs.name)) {
        // unary operator
        if (args.length === 1) {
          if (lhs.name === '-') {
            return `-${Expr.show(args[0])}`;
          }

          return `${lhs.name} ${Expr.show(args[0])}`;
        }

        // binary operator
        if (args.length === 2) {
          return `${Expr.show(args[0])} ${lhs.name} ${Expr.show(args[1])}`;
        }
      }

      if (lhs.variant === 'Variable' && lhs.name === '[]') {
        return `${Expr.show(args[0])}[${joinWith(args.slice(1), Expr.show)}]`;
      }

      if (lhs.variant === 'Variable' && lhs.name === '[]=') {
        return `${Expr.show(args[0])}[${joinWith(args.slice(1, -1), Expr.show)}] = ${Expr.show(last(args))}`;
      }

      const params = `${typeParams.length > 0 ? `<${joinWith(typeParams, MonoTy.show)}>` : ''}`;
      return `${Expr.show(lhs)}${params}(${joinWith(args, Expr.show, ', ')})`;
    },
    Error: ({ message }) => `<Error: ${message}>`,
    Closure: ({ args, body }) => `${ArgumentList.show(args)} -> ${Expr.show(body)}`,
    Block: ({ statements, lastExpr }) => `{\n${joinWith([...statements, ...lastExpr.mapWithDefault(e => [Stmt.Expr(e)], [])], s => '  ' + Stmt.show(s), '\n')}\n}`,
    IfThenElse: ({ condition, then, elseifs, else_ }) => `if ${Expr.show(condition)} ${Expr.show(then)}${elseifs.map(({ cond, body }) => ` else if ${Expr.show(cond)} ${Expr.show(body)}`).join('\n')}${else_.map(e => ` else ${Expr.show(e)}`).orDefault('')}`,
    FieldAccess: ({ lhs, field }) => `${Expr.show(lhs)}.${field}`,
    Tuple: ({ elements }) => `(${joinWith(elements, Expr.show, ', ')})`,
    Array: ({ init }) => ArrayInit.show(init),
    Match: ({ expr, cases }) => `match ${Expr.show(expr)} {\n${joinWith(cases, ({ pattern, body }) => `  ${Pattern.show(pattern)} => ${Expr.show(body)}\n`, '\n')}\n}`,
    Parenthesized: ({ expr }) => `(${Expr.show(expr)})`,
    Struct: ({ name, typeParams, fields }) => {
      const tyParamsFmt = typeParams.length > 0 ? `<${joinWith(typeParams, MonoTy.show, ', ')}>` : '';
      const inline = fields.length <= 2;
      let fieldsFmt = joinWith(fields, ({ name, value }) => `${name}: ${Expr.show(value)}`, inline ? ', ' : ',\n');
      fieldsFmt = inline ? `{ ${fieldsFmt} }` : `{\n${fieldsFmt}\n}`;
      return `${name}${tyParamsFmt} ${fieldsFmt}`;
    },
    TupleIndexing: ({ lhs, index }) => `${Expr.show(lhs)}.${index}`,
    LetIn: ({ pattern, value, body }) => `let ${Pattern.show(pattern)} = ${Expr.show(value)} in ${Expr.show(body)}`,
  }),
};

export type Pattern = DataType<{
  Const: { value: Const },
  Variable: { name: string },
  Tuple: { elements: Pattern[] },
  Any: {},
  Error: { message: string },
}>;

export const Pattern = {
  Const: (value: Const): Pattern => ({ variant: 'Const', value }),
  Variable: (name: string): Pattern => ({ variant: 'Variable', name }),
  Tuple: (elements: Pattern[]): Pattern => ({ variant: 'Tuple', elements }),
  Any: (): Pattern => ({ variant: 'Any' }),
  Error: (message: string): Pattern => ({ variant: 'Error', message }),
  show: (pattern: Pattern): string => match(pattern, {
    Const: ({ value }) => `${Const.show(value)}`,
    Variable: ({ name }) => `${name}`,
    Tuple: ({ elements }) => `(${joinWith(elements, Pattern.show)})`,
    Any: () => '_',
    Error: ({ message }) => `<Error: ${message}>`,
  }),
  isIrrefutable: (pattern: Pattern): boolean => match(pattern, {
    Const: () => false,
    Variable: () => true,
    Tuple: ({ elements }) => elements.every(Pattern.isIrrefutable),
    Any: () => true,
    Error: () => false,
  }),
};

export type Stmt = DataType<{
  Let: {
    name: string,
    expr: Expr,
    mutable: boolean,
    annotation: Maybe<MonoTy>
  },
  Assignment: { lhs: Expr, rhs: Expr },
  Expr: { expr: Expr },
  While: { condition: Expr, statements: Stmt[] },
  Return: { expr: Maybe<Expr> },
  Error: { message: string },
}>;

export const Stmt = {
  Let: (name: string, expr: Expr, mutable: boolean, annotation: Maybe<MonoTy>): Stmt => ({ variant: 'Let', name, expr, mutable, annotation }),
  Assignment: (lhs: Expr, rhs: Expr): Stmt => ({ variant: 'Assignment', lhs, rhs }),
  Expr: (expr: Expr): Stmt => ({ variant: 'Expr', expr }),
  While: (condition: Expr, statements: Stmt[]): Stmt => ({ variant: 'While', condition, statements }),
  Return: (expr: Maybe<Expr>): Stmt => ({ variant: 'Return', expr }),
  Error: (message: string): Stmt => ({ variant: 'Error', message }),
  show: (stmt: Stmt): string => match(stmt, {
    Let: ({ name, expr, mutable, annotation }) => `${mutable ? 'mut' : 'let'} ${name}${annotation.mapWithDefault(ty => ': ' + MonoTy.show(ty), '')} = ${Expr.show(expr)}`,
    Assignment: ({ lhs, rhs }) => `${Expr.show(lhs)} = ${Expr.show(rhs)}`,
    Expr: ({ expr }) => Expr.show(expr),
    While: ({ condition, statements }) => `while ${Expr.show(condition)} {\n${statements.map(Stmt.show).join('\n')}\n}`,
    Return: ({ expr }) => `return ${expr.mapWithDefault(Expr.show, '')}`,
    Error: ({ message }) => `<Error: ${message}>`,
  })
};

export type Imports = DataType<{
  names: { names: Set<string> },
  all: {},
}>;

export const Imports = {
  names: (names: string[]): Imports => ({ variant: 'names', names: new Set(names) }),
  all: (): Imports => ({ variant: 'all' }),
  show: (importType: Imports): string => match(importType, {
    names: ({ names }) => `[${[...names].join(', ')}]`,
    all: () => '',
  }),
};

export type Decl = DataType<{
  Function: {
    attributes: Attribute[],
    pub: boolean,
    name: string,
    typeParams: TypeParam[],
    args: Argument[],
    returnTy: Maybe<MonoTy>,
    body: Maybe<Expr>,
  },
  TypeAlias: {
    pub: boolean, name: string, typeParams: TypeParam[], alias: MonoTy
  },
  Import: { isExport: boolean, readonly path: string, resolvedPath: string, imports: Imports },
  Attributes: { attributes: Attribute[] },
  Error: { message: string },
}>;

export const Decl = {
  ...genConstructors<Decl>(['Function', 'TypeAlias', 'Import', 'Attributes', 'Error']),
  show: (decl: Decl): string => match(decl, {
    Function: ({ attributes, name, typeParams, args, returnTy, body }) => {
      const attrsFmt = attributes.length > 0 ? Attribute.showMany(attributes, 'outer') + '\n' : '';
      const argsFmt = `${TypeParams.show(typeParams)}(${joinWith(args, Argument.show, ', ')})`;
      const retTyFmt = returnTy.mapWithDefault(ty => ': ' + MonoTy.show(ty), '');
      const bodyFmt = body.mapWithDefault(Expr.show, '');
      return `${attrsFmt}fun ${name}${argsFmt}${retTyFmt} ${bodyFmt}`;
    },
    TypeAlias: ({ name, typeParams, alias }) => `type ${name}${TypeParams.show(typeParams)} = ${MonoTy.show(alias)} `,
    Attributes: ({ attributes }) => Attribute.showMany(attributes, 'inner'),
    Import: ({ path, imports }) => `import ${path}${Imports.show(imports)} `,
    Error: ({ message }) => `< Error: ${message}> `,
  }),
};

export type Module = {
  name: string,
  path: string,
  decls: Decl[],
  members: Map<string, VariantOf<Decl, 'Function' | 'TypeAlias'>[]>,
  imports: Map<string, { sourceMod: string, isExport: boolean }>,
  attributes: Map<string, string[]>,
};

export type Prog = {
  modules: Map<string, Module>,
  entry: Module,
};

export const Prog = {
  show: (prog: Prog): string => joinWith(prog.entry.decls, Decl.show, '\n\n'),
};