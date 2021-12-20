import fc from 'fast-check';
import { Keyword } from '../../parse/token';
import { isAlpha, isAlphaNum, isLowerCaseLetter, isUpperCase } from '../../utils/strings';

export const lowerAlpha = fc.char().filter(isLowerCaseLetter);
export const upperAlpha = fc.char().filter(isUpperCase);
export const alpha = fc.char().filter(isAlpha);
export const alphaNum = fc.char().filter(isAlphaNum);

export const lowerIdent = fc.tuple(lowerAlpha, fc.array(alphaNum, { minLength: 0 }))
  .map(([h, tl]) => h + tl.join(''))
  .filter(ident => !Keyword.is(ident));