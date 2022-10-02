import { last } from "../utils/array";
import { error, ok, Result } from "../utils/result";
import { Char, isAlpha, isAlphaNum, isDigit } from "../utils/strings";
import { Const, Keyword, Position, Token } from "./token";

const INSERT_SEMICOLONS = false;

export const lex = (source: string, path: string): Result<Token[], string> => {
  return Lexer(source, path).lex();
};

const Lexer = (source: string, path: string) => {
  let index = 0;
  const tokens: Token[] = [];
  const pos: Position = { line: 1, column: 1, path };
  const compoundOpIdents = new Set(['not', 'mod', 'and', 'or', 'nand', 'nor', 'xor', 'xnor']);

  const advance = (): Char | undefined => {
    if (index >= source.length) {
      return undefined;
    }

    const c = source[index];
    index += 1;
    pos.column += 1;

    if (c === '\n') {
      pos.line += 1;
      pos.column = 1;

      if (INSERT_SEMICOLONS && shouldInsertSemicolon()) {
        tokens.push(Token.Symbol(';', { ...pos }));
      }
    }

    return c;
  };

  const peek = (): Char | undefined => source[index];

  const match = (ch: Char): boolean => {
    if (peek() === ch) {
      advance();
      return true;
    }

    return false;
  };

  const matchString = (str: string): boolean => {
    const matches = source.slice(index, index + str.length) === str;

    if (matches) {
      index += str.length;
    }

    return matches;
  };

  const skipWhitespaces = (): void => {
    while (true) {
      const c = peek();
      if (c === ' ' || c === '\t' || c === '\n' || c === '\r') {
        advance();
      } else {
        break;
      }
    }
  };

  const skipLine = (): void => {
    while (true) {
      const c = peek();
      if (c === undefined) {
        break;
      }

      advance();

      if (c === '\n') {
        break;
      }
    }
  };

  const parseNumber = (startIndex: number): Const => {
    while (true) {
      const c = peek();
      if (c !== undefined && isDigit(c)) {
        advance();
      } else {
        break;
      }
    }

    const lexeme = source.slice(startIndex, index);
    let type: `${'u' | 'i'}${8 | 32 | 64}` = 'i32';

    switch (peek()) {
      case 'u':
        if (matchString('u32')) {
          type = 'u32';
        } else if (matchString('u64')) {
          type = 'u64';
        } else if (matchString('u8')) {
          type = 'u8';
        }
        break;
      case 'i':
        if (matchString('i64')) {
          type = 'i64';
        } else if (matchString('i8')) {
          type = 'i8';
        }
        break;
    }

    return Const[type](Number(lexeme));
  };

  const parseString = (startIndex: number): string => {
    while (true) {
      const c = peek();
      if (c === undefined) {
        break;
      }

      advance();

      if (c === '"') {
        break;
      }
    }

    return source.slice(startIndex + 1, index - 1);
  };

  const parseIdentOrKeyword = (startIndex: number): Token => {
    while (true) {
      const c = peek();
      if (c === undefined) {
        break;
      }

      if (isAlphaNum(c)) {
        advance();
      } else {
        break;
      }
    }

    let lexeme = source.slice(startIndex, index);
    if (match('=') && compoundOpIdents.has(lexeme)) {
      lexeme += '=';
    }

    if (lexeme === 'true' || lexeme === 'false') {
      return Token.Const(Const.bool(lexeme === 'true'), { ...pos });
    }

    if (Keyword.is(lexeme)) {
      return Token.Keyword(lexeme, { ...pos });
    }

    return Token.Identifier(lexeme, { ...pos });
  };

  // https://medium.com/golangspec/automatic-semicolon-insertion-in-go-1990338f2649
  const shouldInsertSemicolon = (): boolean => {
    if (tokens.length === 0) {
      return false;
    }

    const lastToken = last(tokens);

    switch (lastToken.variant) {
      case 'Const':
        return true;
      case 'Symbol':
        return lastToken.value === ')' || lastToken.value === ']' || lastToken.value === '}';
      case 'Keyword':
        return lastToken.value === 'return';
    }

    return false;
  };

  const nextToken = (): boolean => {
    skipWhitespaces();
    const startIndex = index;
    const c = advance();

    if (c === undefined) {
      tokens.push(Token.EOF({ ...pos }));
      return false;
    }

    switch (c) {
      case '(':
      case ')':
      case '[':
      case ']':
      case '{':
      case '}':
      case ',':
      case ':':
      case ';':
      case '\'':
      case '_':
      case '#':
        tokens.push(Token.Symbol(c, { ...pos }));
        return true;
      case '.': {
        let token: Token;
        if (match('.')) {
          if (match('.')) {
            token = Token.Symbol('...', { ...pos });
          } else {
            token = Token.Symbol('..', { ...pos });
          }
        } else {
          token = Token.Symbol('.', { ...pos });
        }

        tokens.push(token);
        return true;
      }
      case '+':
        tokens.push(Token.Identifier(match('=') ? '+=' : '+', { ...pos }));
        return true;
      case '*':
        tokens.push(Token.Identifier(match('=') ? '*=' : '*', { ...pos }));
        return true;
      case '=': {
        let token: Token;
        if (match('>')) {
          token = Token.Symbol('=>', { ...pos });
        } else if (match('=')) {
          token = Token.Identifier('==', { ...pos });
        } else {
          token = Token.Identifier('=', { ...pos });
        }

        tokens.push(token);
        return true;
      }
      case '<':
        tokens.push(Token.Identifier(match('=') ? '<=' : '<', { ...pos }));
        return true;
      case '>':
        tokens.push(Token.Identifier(match('=') ? '>=' : '>', { ...pos }));
        return true;
      case '/':
        if (match('/')) {
          skipLine();
        } else {
          tokens.push(Token.Identifier(match('=') ? '/=' : '/', { ...pos }));
        }

        return true;
      case '-': {
        let token: Token;
        if (match('>')) {
          token = Token.Symbol('->', { ...pos });
        } else if (match('=')) {
          token = Token.Identifier('-=', { ...pos });
        } else if (isDigit(peek()!)) {
          token = Token.Const(parseNumber(startIndex), { ...pos });
        } else {
          token = Token.Identifier('-', { ...pos });
        }

        tokens.push(token);
        return true;
      }
      case '!':
        tokens.push(match('=') ?
          Token.Identifier('!=', { ...pos }) :
          Token.Symbol('!', { ...pos })
        );
        return true;
      case '"':
        const str = Const.str(parseString(startIndex));
        tokens.push(Token.Const(str, { ...pos }));
        return true;
      default:
        if (isDigit(c)) {
          const n = parseNumber(startIndex);
          tokens.push(Token.Const(n, { ...pos }));
          return true;
        }

        if (isAlpha(c)) {
          tokens.push(parseIdentOrKeyword(startIndex));
          return true;
        }

        return false;
    }
  };

  const lex = (): Result<Token[], string> => {
    while (true) {
      const shouldContinue = nextToken();
      if (!shouldContinue) {
        break;
      }
    }

    if (index < source.length) {
      return error(`Invalid token '${peek()}' at ${Position.show(pos)}`);
    }

    return ok(tokens);
  };

  return { lex };
};
