import { DataType } from "itsamatch";
import { BitterConversionError } from "../ast/bitter";
import { TypingError } from "../infer/infer";
import { UnificationError } from "../infer/unification";
import { ParserError } from "../parse/combinators";
import { ResolutionError } from '../resolve/resolve';

export type Error = DataType<{
  Resolution: ResolutionError,
  Parser: ParserError,
  BitterConversion: BitterConversionError,
  Typing: TypingError,
  Unification: UnificationError,
}>;

export const Error = {
  Resolution: (err: ResolutionError): Error => ({ variant: 'Resolution', ...err }),
  Parser: (err: ParserError): Error => ({ variant: 'Parser', ...err }),
  BitterConversion: (err: BitterConversionError): Error => ({ variant: 'BitterConversion', ...err }),
  Typing: (err: TypingError): Error => ({ variant: 'Typing', ...err }),
  Unification: (err: UnificationError): Error => ({ variant: 'Unification', ...err }),
};