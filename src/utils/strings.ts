
export type Char = string;

export const isUpperCase = (str: string): boolean => str === str.toUpperCase();
export const isLowerCase = (str: string): boolean => str === str.toLowerCase();
export const isLowerCaseLetter = (c: Char) => c >= 'a' && c <= 'z';
export const isUpperCaseLetter = (c: Char) => c >= 'A' && c <= 'Z';
export const isAlpha = (c: Char) => isLowerCaseLetter(c) || isUpperCaseLetter(c);
export const isDigit = (c: Char) => c >= '0' && c <= '9';
export const isAlphaNum = (c: Char) => isAlpha(c) || isDigit(c);