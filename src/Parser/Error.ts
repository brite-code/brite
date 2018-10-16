import {Expression} from './Ast';
import {Glyph, Token} from './Lexer';

export type ParserError = UnexpectedTokenError | ExpressionIntoPatternError;

export const enum ParserErrorType {
  UnexpectedToken = 'UnexpectedToken',
  ExpressionIntoPattern = 'ExpressionIntoPattern',
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

export type ExpressionIntoPatternError = {
  readonly type: ParserErrorType.ExpressionIntoPattern;
  readonly expression: Expression;
  readonly reason: Token;
};

export function ExpressionIntoPatternError(
  expression: Expression,
  reason: Token
): ExpressionIntoPatternError {
  return {
    type: ParserErrorType.ExpressionIntoPattern,
    expression,
    reason,
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
  | ExpectedBindingIdentifier
  | ExpectedKeyword
  | ExpectedGlyph
  | ExpectedEnd
  | ExpectedLineSeparator
  | ExpectedDeclaration
  | ExpectedClassMember
  | ExpectedInterfaceMember
  | ExpectedType
  | ExpectedExpression
  | ExpectedPattern;

export const enum ExpectedKind {
  Identifier = 'Identifier',
  BindingIdentifier = 'BindingIdentifier',
  Keyword = 'Keyword',
  Glyph = 'Glyph',
  End = 'End',
  LineSeparator = 'LineSeparator',
  Declaration = 'Declaration',
  ClassMember = 'ClassMember',
  InterfaceMember = 'InterfaceMember',
  Type = 'Type',
  Expression = 'Expression',
  Pattern = 'Pattern',
}

export type ExpectedIdentifier = {readonly kind: ExpectedKind.Identifier};
export const ExpectedIdentifier: ExpectedIdentifier = {
  kind: ExpectedKind.Identifier,
};

export type ExpectedBindingIdentifier = {
  readonly kind: ExpectedKind.BindingIdentifier;
};
export const ExpectedBindingIdentifier: ExpectedBindingIdentifier = {
  kind: ExpectedKind.BindingIdentifier,
};

export type ExpectedKeyword = {
  readonly kind: ExpectedKind.Keyword;
  readonly keyword: string;
};

export function ExpectedKeyword(keyword: string): ExpectedKeyword {
  return {kind: ExpectedKind.Keyword, keyword};
}

export type ExpectedGlyph = {
  readonly kind: ExpectedKind.Glyph;
  readonly glyph: Glyph;
};

export function ExpectedGlyph(glyph: Glyph): ExpectedGlyph {
  return {kind: ExpectedKind.Glyph, glyph};
}

export type ExpectedEnd = {readonly kind: ExpectedKind.End};
export const ExpectedEnd: ExpectedEnd = {kind: ExpectedKind.End};

export type ExpectedLineSeparator = {readonly kind: ExpectedKind.LineSeparator};
export const ExpectedLineSeparator: ExpectedLineSeparator = {
  kind: ExpectedKind.LineSeparator,
};

export type ExpectedDeclaration = {readonly kind: ExpectedKind.Declaration};
export const ExpectedDeclaration: ExpectedDeclaration = {
  kind: ExpectedKind.Declaration,
};

export type ExpectedClassMember = {readonly kind: ExpectedKind.ClassMember};
export const ExpectedClassMember: ExpectedClassMember = {
  kind: ExpectedKind.ClassMember,
};

export type ExpectedInterfaceMember = {
  readonly kind: ExpectedKind.InterfaceMember;
};
export const ExpectedInterfaceMember: ExpectedInterfaceMember = {
  kind: ExpectedKind.InterfaceMember,
};

export type ExpectedType = {readonly kind: ExpectedKind.Type};
export const ExpectedType: ExpectedType = {kind: ExpectedKind.Type};

export type ExpectedExpression = {readonly kind: ExpectedKind.Expression};
export const ExpectedExpression: ExpectedExpression = {
  kind: ExpectedKind.Expression,
};

export type ExpectedPattern = {readonly kind: ExpectedKind.Pattern};
export const ExpectedPattern: ExpectedPattern = {kind: ExpectedKind.Pattern};
