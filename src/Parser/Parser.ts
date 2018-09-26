import {Type, TypeType} from './Ast';
import {ExpectedType, ParserError, UnexpectedTokenError} from './Error';
import {Identifier} from './Identifier';
import {Glyph, Lexer, TokenType} from './Lexer';

export function parseType(
  lexer: Lexer
): {
  readonly errors: ReadonlyArray<ParserError>;
  readonly type: Type;
} {
  const parser = new Parser(lexer);
  const type = parser.parseType();
  const errors = parser.getErrors();
  return {errors, type};
}

class Parser {
  private readonly lexer: Lexer;
  private readonly errors: Array<ParserError> = [];

  constructor(lexer: Lexer) {
    this.lexer = lexer;
  }

  reportError(error: ParserError) {
    this.errors.push(error);
  }

  getErrors(): ReadonlyArray<ParserError> {
    return this.errors;
  }

  parseType(): Type {
    const token = this.lexer.peek();
    // if (token.type === TokenType.Glyph && token.glyph === Glyph.ParenLeft) {
    //   this.lexer.next();
    //   return; // TODO: parse function
    // }
    // if (
    //   token.type === TokenType.Identifier &&
    //   Identifier.getBindingKeyword(token.identifier)
    // ) {
    //   this.lexer.next();
    //   return; // TODO: parse function shorthand
    // }
    // if (token.type === TokenType.Glyph && token.glyph === Glyph.LessThan) {
    //   this.lexer.next();
    //   return; // TODO: parse type quantification
    // }
    return this.parsePrimaryType();
  }

  parsePrimaryType(): Type {
    const token = this.lexer.next();
    if (
      token.type === TokenType.Identifier &&
      !Identifier.getBindingKeyword(token.identifier)
    ) {
      return {
        type: TypeType.Reference,
        loc: token.loc,
        identifier: token.identifier,
      };
    }
    if (token.type === TokenType.Glyph && token.glyph === Glyph.ParenLeft) {
    }
    const error = UnexpectedTokenError(token, {type: ExpectedType.Type});
    this.reportError(error);
    return {type: TypeType.Error, loc: error.unexpected.loc, error};
  }

  // TODO: Document and test!
  parseCommaList<T>(parseItem: () => T, tryParseEnd: () => boolean): Array<T> {
    if (tryParseEnd()) return [];
    const items = [parseItem()];
    while (true) {
      if (tryParseEnd()) break;
      this.parseGlyph(Glyph.Comma);
      if (tryParseEnd()) break;
      items.push(parseItem());
    }
    return items;
  }

  parseGlyph(glyph: Glyph): UnexpectedTokenError | undefined {
    const token = this.lexer.peek();
    if (token.type === TokenType.Glyph && token.glyph === glyph) {
      this.lexer.next();
      return undefined;
    }
    const e = UnexpectedTokenError(token, {type: ExpectedType.Glyph, glyph});
    this.reportError(e);
    return e;
  }
}
