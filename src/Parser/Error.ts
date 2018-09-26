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

/**
 * A description of what we expected when we encounter an unexpected token.
 *
 * We recreate some of `Token` since we don’t want to provide a `Loc` with an
 * expectation description.
 */
export type Expected =
  | {readonly type: ExpectedType.Identifier}
  | {readonly type: ExpectedType.Glyph; readonly glyph: Glyph}
  | {readonly type: ExpectedType.End}
  | {readonly type: ExpectedType.Type};

export const enum ExpectedType {
  Identifier = 'Identifier',
  Glyph = 'Glyph',
  End = 'End',
  Type = 'Type',
}
