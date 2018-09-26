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
  | ExpectedIdentifier
  | ExpectedGlyph
  | ExpectedEnd
  | ExpectedType;

export const enum ExpectedKind {
  Identifier = 'Identifier',
  Glyph = 'Glyph',
  End = 'End',
  Type = 'Type',
}

export type ExpectedIdentifier = {readonly kind: ExpectedKind.Identifier};

export const ExpectedIdentifier: ExpectedIdentifier = {
  kind: ExpectedKind.Identifier,
};

export type ExpectedGlyph = {
  readonly kind: ExpectedKind.Glyph;
  readonly glyph: Glyph;
};

export function ExpectedGlyph(glyph: Glyph): ExpectedGlyph {
  return {kind: ExpectedKind.Glyph, glyph};
}

export type ExpectedEnd = {readonly kind: ExpectedKind.End};
export const ExpectedEnd: ExpectedEnd = {kind: ExpectedKind.End};

export type ExpectedType = {readonly kind: ExpectedKind.Type};
export const ExpectedType: ExpectedType = {kind: ExpectedKind.Type};
