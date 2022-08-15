import { DataType, match as matchVariant } from "itsamatch";
import { BitterConversionError, Expr } from "../ast/bitter";
import { TypingError } from "../infer/infer";
import { MAX_TUPLE_INDEX, Tuple } from "../infer/tuples";
import { MonoTy, PolyTy } from "../infer/types";
import { UnificationError } from "../infer/unification";
import { ParserError } from "../parse/combinators";
import { Position } from "../parse/token";
import { ResolutionError } from '../resolve/resolve';

export type Error = DataType<{
  Resolution: { err: ResolutionError },
  Parser: { err: ParserError },
  BitterConversion: { err: BitterConversionError },
  Typing: { err: TypingError },
  Unification: { err: UnificationError },
}>;

const formatOverloadingCandidates = (types: PolyTy[]): string => {
  return types.map(ty => `  ${PolyTy.show(ty)}`).join('\n');
};

export const Error = {
  Resolution: (err: ResolutionError): Error => ({ variant: 'Resolution', err }),
  Parser: (err: ParserError): Error => ({ variant: 'Parser', err }),
  BitterConversion: (err: BitterConversionError): Error => ({ variant: 'BitterConversion', err }),
  Typing: (err: TypingError): Error => ({ variant: 'Typing', err }),
  Unification: (err: UnificationError): Error => ({ variant: 'Unification', err }),
  show: (err: Error): string => matchVariant(err, {
    Unification: ({ err }) => matchVariant(err, {
      Ununifiable: ({ s, t }) => `Cannot unify ${MonoTy.show(s)} with ${MonoTy.show(t)}`,
      RecursiveType: ({ s, t }) => `Recursive type encountered: ${MonoTy.show(s)} with ${MonoTy.show(t)}`,
      UnknownStructField: ({ row, field }) => `Unknown field '${field}' in struct ${MonoTy.show(MonoTy.Struct(row))}`,
      DifferentLengthTuples: ({ s, t }) => `Cannot unify tuples with different lengths: ${Tuple.show(s)} with ${Tuple.show(t)}`,
      CouldNotResolveType: ({ ty }) => `Could not resolve type: ${MonoTy.show(ty)}`,
    }, 'type'),
    Parser: ({ err }) => `Parser error: ${err.message} at ${Position.show(err.pos)}`,
    BitterConversion: ({ err }) => `Bitter conversion error: ${err.message}`,
    Typing: ({ err }) => matchVariant(err, {
      ParsingError: ({ message }) => `Parsing error: ${message}`,
      UnboundVariable: ({ name }) => `Unbound variable: '${name}'`,
      UnknownFunction: ({ name }) => `Unknown function: '${name}'`,
      ImmutableValue: ({ expr }) => `Cannot update immutable value '${Expr.showSweet(expr)}'`,
      UnassignableExpression: ({ expr }) => `${Expr.showSweet(expr)} is not an assignable expression`,
      UnknownModule: ({ path }) => `Unknown module '${path.join('.')}'`,
      UnknownModuleMember: ({ path, member }) => `Unknown member '${member}' in module '${path.join('.')}'`,
      TEMP_OnlyFunctionModuleAccessAllowed: ({ path }) => `(temp) Only functions can be accessed from module '${path.join('.')}'`,
      TupleIndexTooBig: ({ index }) => `Tuple index ${index} is too big, limit is ${MAX_TUPLE_INDEX}`,
      WasmBlockExpressionsRequireTypeAnnotations: ({ expr }) => `Expressions inside a wasm block require type annotations, type of '${Expr.showSweet(expr)}' is not fully determined`,
      NoOverloadMatchesCallSignature: ({ name, f, candidates }) => `No overload of '${name}' matches the call signature '${MonoTy.show(f)}', candidates:\n${formatOverloadingCandidates(candidates)}`,
      AmbiguousOverload: ({ name, f, matches }) => `Ambiguous overload for '${name}' with call signature ${MonoTy.show(f)}, matches:\n${formatOverloadingCandidates(matches)}`,
      UndeclaredStruct: ({ name }) => `Undeclared struct '${name}' in struct constructor expression`,
      MissingStructFields: ({ name, fields }) => `Missing fields in struct constructor expression for '${name}': ${fields.join(', ')}`,
      ExtraneoussStructFields: ({ name, fields }) => `Extraneous fields in struct constructor expression for '${name}': ${fields.join(', ')}`,
      CannotUseImmutableValueForImmutableFuncArg: ({ func, arg }) => `Cannot use an immutable value in place of an immutable argument: '${arg}' in function '${func}'`,
      MissingFuncPrototypeReturnTy: ({ name }) => `Missing return type in function prototype for ${name}`,
    }, 'type'),
    Resolution: ({ err }) => matchVariant(err, {
      ModuleNotFound: ({ name }) => `Module '${name}' not found`,
      ModuleAlreadyExists: ({ name, path, existingPath }) => `Module '${name}' already exists at '${path}', but was already imported at '${existingPath}'`,
      TypeNotFound: ({ name, path }) => `Type '${[...path, name].join('.')}' not found`,
    }, 'type'),
  }),
};