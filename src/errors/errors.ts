import { DataType, match as matchVariant } from "itsamatch";
import { BitterConversionError, Expr } from "../ast/bitter";
import { TypingError } from "../infer/infer";
import { MAX_TUPLE_INDEX, Tuple } from "../infer/tuples";
import { MonoTy, PolyTy } from "../infer/types";
import { UnificationError } from "../infer/unification";
import { ParserError } from "../parse/combinators";
import { Position } from "../parse/token";
import { FileSystem } from "../resolve/fileSystem";
import { ResolutionError } from '../resolve/resolve';

type WithOptionalPos<T> = { [K in keyof T]: T[K] & { pos?: Position } };

export type Error = DataType<WithOptionalPos<{
  Resolution: { err: ResolutionError },
  Parser: { err: ParserError },
  BitterConversion: { err: BitterConversionError },
  Typing: { err: TypingError },
  Unification: { err: UnificationError },
}>>;

const formatOverloadingCandidates = (types: PolyTy[]): string => {
  return types.map(ty => `  ${PolyTy.show(ty)}`).join('\n');
};

export const Error = {
  Resolution: (err: ResolutionError): Error => ({ variant: 'Resolution', err }),
  Parser: (err: ParserError): Error => ({ variant: 'Parser', err }),
  BitterConversion: (err: BitterConversionError): Error => ({ variant: 'BitterConversion', err }),
  Typing: (err: TypingError): Error => ({ variant: 'Typing', err }),
  Unification: (err: UnificationError): Error => ({ variant: 'Unification', err }),
  show: async (err: Error, fs: FileSystem): Promise<string> => {
    const formatted = matchVariant(err, {
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
        TupleIndexTooBig: ({ index }) => `Tuple index ${index} is too big, limit is ${MAX_TUPLE_INDEX}`,
        NoOverloadMatchesCallSignature: ({ name, f, candidates }) => `No overload of '${name}' matches the call signature '${MonoTy.show(f)}', candidates:\n${formatOverloadingCandidates(candidates)}`,
        AmbiguousOverload: ({ name, funTy, matches }) => `Ambiguous overload for '${name}' with call signature ${funTy}, matches:\n${formatOverloadingCandidates(matches)}`,
        UndeclaredStruct: ({ name }) => `Undeclared struct '${name}' in struct constructor expression`,
        MissingStructFields: ({ name, fields }) => `Missing fields in struct constructor expression for '${name}': ${fields.join(', ')}`,
        ExtraneoussStructFields: ({ name, fields }) => `Extraneous fields in struct constructor expression for '${name}': ${fields.join(', ')}`,
        CannotUseImmutableValueForMutFuncArg: ({ func, arg }) => `Cannot use an immutable value in place of a mutable argument: '${arg}' in function '${func}'`,
        MissingFuncPrototypeReturnTy: ({ name }) => `Missing return type in function prototype for '${name}'`,
        ReturnUsedOutsideFunctionBody: () => `Return expression used outside of a function body`,
        IncorrectNumberOfTypeParams: ({ name, given, expected }) => `Incorrect number of type parameters for function '${name}', expected ${expected}, got ${given}`,
        InvalidMainFunSignature: ({ ty }) => `Invalid signature for the main function, expected () -> void, got ${PolyTy.show(ty)}`,
      }, 'type'),
      Resolution: ({ err }) => matchVariant(err, {
        ModuleNotFound: ({ name }) => `Module '${name}' not found`,
        UnknownMember: ({ modulePath, member }) => `Module '${modulePath}' has no exported member '${member}'`,
        MemberIsNotPublic: ({ modulePath, member }) => `Member '${member}' from module '${modulePath}' is not public`,
      }),
    });

    if (err.pos) {
      const source = await fs.readFile(err.pos.path);
      const lines = source.split('\n');
      const errLine = lines[err.pos.line - 1];
      const highlight = ' '.repeat(err.pos.column - 1) + '^';
      return `${formatted}\n\n${errLine}\n${highlight} ${Position.showWithShortPath(err.pos, fs)}`;
    }

    return formatted;
  },
};