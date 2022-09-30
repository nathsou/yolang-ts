import { last } from "../utils/array";
import { error, ok, Result } from "../utils/result";
import { Char, isAlpha, isAlphaNum, isDigit } from "../utils/strings";
import { Const, Keyword, Position, Token, TokenWithPos, withPos } from "./token";

const INSERT_SEMICOLONS = false;

export const lex = (source: string): Result<TokenWithPos[], string> => {
  const lexer = Lexer.make(source);
  return Lexer.lex(lexer);
};

type Lexer = {
  source: string,
  index: number,
  tokens: TokenWithPos[],
  pos: Position,
};

const Lexer = {
  compoundOpIdents: new Set(['not', 'mod', 'and', 'or', 'nand', 'nor', 'xor', 'xnor']),
  make: (source: string): Lexer => ({
    source,
    index: 0,
    tokens: [],
    pos: { line: 1, column: 1 },
  }),
  advance: (self: Lexer): Char | undefined => {
    if (self.index >= self.source.length) {
      return undefined;
    }

    const c = self.source[self.index];
    self.index += 1;
    self.pos.column += 1;

    if (c === '\n') {
      self.pos.line += 1;
      self.pos.column = 1;

      if (INSERT_SEMICOLONS && Lexer.shouldInsertSemicolon(self)) {
        self.tokens.push(withPos(Token.Symbol(';'), self.pos));
      }
    }

    return c;
  },
  peek: (self: Lexer): Char | undefined => {
    return self.source[self.index];
  },
  match: (self: Lexer, ch: Char): boolean => {
    if (Lexer.peek(self) === ch) {
      Lexer.advance(self);
      return true;
    }

    return false;
  },
  matchString: (self: Lexer, str: string): boolean => {
    const matches = self.source.slice(self.index, self.index + str.length) === str;

    if (matches) {
      self.index += str.length;
    }

    return matches;
  },
  skipWhitespaces: (self: Lexer): void => {
    while (true) {
      const c = Lexer.peek(self);
      if (c === ' ' || c === '\t' || c === '\n' || c === '\r') {
        Lexer.advance(self);
      } else {
        break;
      }
    }
  },
  skipLine: (self: Lexer): void => {
    while (true) {
      const c = Lexer.peek(self);
      if (c === undefined) {
        break;
      }

      Lexer.advance(self);

      if (c === '\n') {
        break;
      }
    }
  },
  parseNumber: (self: Lexer, startIndex: number): Const => {
    while (true) {
      const c = Lexer.peek(self);
      if (c !== undefined && isDigit(c)) {
        Lexer.advance(self);
      } else {
        break;
      }
    }

    const lexeme = self.source.slice(startIndex, self.index);
    let type: `${'u' | 'i'}${8 | 32 | 64}` = 'i32';

    switch (Lexer.peek(self)) {
      case 'u':
        if (Lexer.matchString(self, 'u32')) {
          type = 'u32';
        } else if (Lexer.matchString(self, 'u64')) {
          type = 'u64';
        } else if (Lexer.matchString(self, 'u8')) {
          type = 'u8';
        }
        break;
      case 'i':
        if (Lexer.matchString(self, 'i64')) {
          type = 'i64';
        } else if (Lexer.matchString(self, 'i8')) {
          type = 'i8';
        }
        break;
    }

    return Const[type](Number(lexeme));
  },
  parseString: (self: Lexer, startIndex: number): string => {
    while (true) {
      const c = Lexer.peek(self);
      if (c === undefined) {
        break;
      }

      Lexer.advance(self);

      if (c === '"') {
        break;
      }
    }

    return self.source.slice(startIndex + 1, self.index - 1);
  },
  parseIdentOrKeyword: (self: Lexer, startIndex: number): Token => {
    while (true) {
      const c = Lexer.peek(self);
      if (c === undefined) {
        break;
      }

      if (isAlphaNum(c)) {
        Lexer.advance(self);
      } else {
        break;
      }
    }

    let lexeme = self.source.slice(startIndex, self.index);
    if (Lexer.match(self, '=') && Lexer.compoundOpIdents.has(lexeme)) {
      lexeme += '=';
    }

    if (lexeme === 'true' || lexeme === 'false') {
      return Token.Const(Const.bool(lexeme === 'true'));
    }

    if (Keyword.is(lexeme)) {
      return Token.Keyword(lexeme);
    }

    return Token.Identifier(lexeme);
  },
  // https://medium.com/golangspec/automatic-semicolon-insertion-in-go-1990338f2649
  shouldInsertSemicolon: (self: Lexer): boolean => {
    if (self.tokens.length === 0) {
      return false;
    }

    const lastToken = last(self.tokens);

    switch (lastToken.variant) {
      case 'Const':
        return true;
      case 'Symbol':
        return lastToken.value === ')' || lastToken.value === ']' || lastToken.value === '}';
      case 'Keyword':
        return lastToken.value === 'return';
    }

    return false;
  },
  nextToken: (self: Lexer): boolean => {
    Lexer.skipWhitespaces(self);
    const startIndex = self.index;
    const c = Lexer.advance(self);

    if (c === undefined) {
      self.tokens.push(withPos(Token.EOF(), self.pos));
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
        self.tokens.push(withPos(Token.Symbol(c), self.pos));
        return true;
      case '.': {
        let token: Token;
        if (Lexer.match(self, '.')) {
          if (Lexer.match(self, '.')) {
            token = Token.Symbol('...');
          } else {
            token = Token.Symbol('..');
          }
        } else {
          token = Token.Symbol('.');
        }

        self.tokens.push(withPos(token, self.pos));
        return true;
      }
      case '+':
        self.tokens.push(withPos(
          Token.Identifier(Lexer.match(self, '=') ? '+=' : '+'),
          self.pos
        ));
        return true;
      case '*':
        self.tokens.push(withPos(
          Token.Identifier(Lexer.match(self, '=') ? '*=' : '*'),
          self.pos
        ));
        return true;
      case '=': {
        let token: Token;
        if (Lexer.match(self, '>')) {
          token = Token.Symbol('=>');
        } else if (Lexer.match(self, '=')) {
          token = Token.Identifier('==');
        } else {
          token = Token.Identifier('=');
        }

        self.tokens.push(withPos(token, self.pos));
        return true;
      }
      case '<':
        self.tokens.push(withPos(
          Token.Identifier(Lexer.match(self, '=') ? '<=' : '<'),
          self.pos
        ));
        return true;
      case '>':
        self.tokens.push(withPos(
          Token.Identifier(Lexer.match(self, '=') ? '>=' : '>'),
          self.pos
        ));
        return true;
      case '/':
        if (Lexer.match(self, '/')) {
          Lexer.skipLine(self);
        } else {
          self.tokens.push(withPos(
            Token.Identifier(Lexer.match(self, '=') ? '/=' : '/'),
            self.pos
          ));
        }

        return true;
      case '-': {
        let token: Token;
        if (Lexer.match(self, '>')) {
          token = Token.Symbol('->');
        } else if (Lexer.match(self, '=')) {
          token = Token.Identifier('-=');
        } else if (isDigit(Lexer.peek(self)!)) {
          token = Token.Const(Lexer.parseNumber(self, startIndex));
        } else {
          token = Token.Identifier('-');
        }

        self.tokens.push(withPos(token, self.pos));
        return true;
      }
      case '!':
        self.tokens.push(withPos(
          Token.Identifier(Lexer.match(self, '=') ? '!=' : '!'),
          self.pos
        ));
        return true;
      case '>':
        self.tokens.push(withPos(
          Token.Identifier(Lexer.match(self, '=') ? '>=' : '>'),
          self.pos
        ));
        return true;
      case '<':
        self.tokens.push(withPos(
          Token.Identifier(Lexer.match(self, '=') ? '<=' : '<'),
          self.pos
        ));
        return true;
      case '"':
        const str = Const.str(Lexer.parseString(self, startIndex));
        self.tokens.push(withPos(Token.Const(str), self.pos));
        return true;
      default:
        if (isDigit(c)) {
          const n = Lexer.parseNumber(self, startIndex);
          self.tokens.push(withPos(Token.Const(n), self.pos));
          return true;
        }

        if (isAlpha(c)) {
          self.tokens.push(withPos(Lexer.parseIdentOrKeyword(self, startIndex), self.pos));
          return true;
        }

        return false;
    }
  },
  lex: (self: Lexer): Result<TokenWithPos[], string> => {
    while (true) {
      const shouldContinue = Lexer.nextToken(self);
      if (!shouldContinue) {
        break;
      }
    }

    if (self.index < self.source.length) {
      return error(`Invalid token '${Lexer.peek(self)}' at ${Position.show(self.pos)}`);
    }

    return ok(self.tokens);
  },
};
