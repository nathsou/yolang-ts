import { DataType, match as matchVariant } from "itsamatch";
import { BitterConversionError } from "../ast/bitter";
import { Expr } from "../ast/bitter";
import { TypingError } from "../infer/infer";
import { MAX_TUPLE_INDEX, Tuple } from "../infer/tuples";
import { MonoTy } from "../infer/types";
import { UnificationError } from "../infer/unification";
import { ParserError } from "../parse/combinators";
import { ResolutionError } from '../resolve/resolve';

export type Error = DataType<{
  Resolution: { err: ResolutionError },
  Parser: { err: ParserError },
  BitterConversion: { err: BitterConversionError },
  Typing: { err: TypingError },
  Unification: { err: UnificationError },
}>;

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
      UnknownRecordField: ({ row, field }) => `Unknown field '${field}' in record ${MonoTy.show(MonoTy.Record(row))}`,
      DifferentLengthTuples: ({ s, t }) => `Cannot unify tuples with different lengths: ${Tuple.show(s)} with ${Tuple.show(t)}`,
      CouldNotResolveType: ({ ty }) => `Could not resolve type: ${MonoTy.show(ty)}`,
    }, 'type'),
    Parser: ({ err }) => `Parser error: ${err.message} at ${err.pos}`,
    BitterConversion: ({ err }) => `Bitter conversion error: ${err.message}`,
    Typing: ({ err }) => matchVariant(err, {
      ParsingError: ({ message }) => `Parsing error: ${message}`,
      UnboundVariable: ({ name }) => `Unbound variable: '${name}'`,
      ImmutableVariable: ({ name }) => `Cannot update immutable variable '${name}`,
      UnassignableExpression: ({ expr }) => `${Expr.showSweet(expr)} is not an assignable expression `,
      UnknownMethod: ({ method, ty }) => `Unknown method '${method}' in type ${MonoTy.show(ty)}`,
      UnknownModule: ({ path }) => `Unknown module '${path.join('.')}'`,
      UnknownModuleMember: ({ path, member }) => `Unknown member '${member}' in module '${path.join('.')}'`,
      TEMP_OnlyFunctionModuleAccessAllowed: ({ path }) => `(temp) Only functions can be accessed from module '${path.join('.')}'`,
      TupleIndexTooBig: ({ index }) => `Tuple index ${index} is too big, limit is ${MAX_TUPLE_INDEX}`,
    }, 'type'),
    Resolution: ({ err }) => matchVariant(err, {
      ModuleNotFound: ({ name }) => `Module '${name}' not found`,
      ModuleAlreadyExists: ({ name, path, existingPath }) => `Module '${name}' already exists at '${path}', but was already imported at '${existingPath}'`,
    }, 'type'),
  }),
};