import {Glyph, Token} from './Lexer';

export type ParserError = UnexpectedTokenError;

export const enum ParserErrorType {
  UnexpectedToken = 'UnexpectedToken',
}

export type UnexpectedTokenError = {
  readonly type: ParserErrorType.UnexpectedToken;
  readonly unexpected: Token;
  readonly expected: Expected;
};

export function UnexpectedTokenError(
  unexpected: Token,
  expected: Expected
): UnexpectedTokenError {
  return {
    type: ParserErrorType.UnexpectedToken,
    unexpected,
    expected,
  };
}

export type Expected =
  | {readonly type: ExpectedType.Type}
  | {readonly type: ExpectedType.Glyph; readonly glyph: Glyph};

export const enum ExpectedType {
  Type = 'Type',
  Glyph = 'Glyph',
}
