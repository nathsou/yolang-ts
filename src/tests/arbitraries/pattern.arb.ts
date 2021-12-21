import fc from 'fast-check';
import { Pattern } from '../../ast/sweet';
import { lowerIdent } from './common.arb';

export const varPat = lowerIdent.map(Pattern.Variable);

export const patternArb = (maxDepth = 5) => fc.oneof({ maxDepth: maxDepth }, varPat);