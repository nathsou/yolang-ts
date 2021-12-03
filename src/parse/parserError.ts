import { Position } from "./token";

export type ParserError = {
  message: string,
  position: Position,
};

export const ParserError = {
  make: (message: string, position: Position): ParserError => ({ message, position }),
};