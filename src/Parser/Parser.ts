import {ReadonlyArray2} from '../Utils/ArrayN';

import {
  ErrorType,
  FunctionType,
  GenericType,
  MemberType,
  Name,
  QuantifiedType,
  RecordType,
  RecordTypeProperty,
  ReferenceType,
  TupleType,
  Type,
  TypeParameter,
  TypeType,
  UnitType,
  WrappedType,
} from './Ast';
import {
  ExpectedEnd,
  ExpectedGlyph,
  ExpectedIdentifier,
  ExpectedType,
  ParserError,
  UnexpectedTokenError,
} from './Error';
import {Identifier} from './Identifier';
import {Glyph, IdentifierToken, Lexer, TokenType} from './Lexer';
import {Loc} from './Loc';

export function parseType(
  lexer: Lexer
): {
  readonly errors: ReadonlyArray<ParserError>;
  readonly type: Type;
} {
  const parser = new Parser(lexer);
  const type = parser.parseType();
  parser.parseEnding();
  const errors = parser.getErrors();
  return {errors, type};
}

export function parseCommaListTest(
  lexer: Lexer
): {
  readonly errors: ReadonlyArray<ParserError>;
  readonly result: ReadonlyArray<string>;
} {
  const parser = new Parser(lexer);
  const result = parser.parseCommaListTest();
  parser.parseEnding();
  const errors = parser.getErrors();
  return {errors, result};
}

/**
 * A parser with error recovery.
 *
 * We try our best to massage a source string into an AST in the face of parse
 * errors. There is no grand theory for parse error recovery. We just do what
 * feels right.
 */
class Parser {
  private readonly lexer: Lexer;
  private readonly errors: Array<ParserError> = [];

  constructor(lexer: Lexer) {
    this.lexer = lexer;
  }

  getErrors(): ReadonlyArray<ParserError> {
    return this.errors;
  }

  /**
   * Parses the `Type` grammar. Advances the lexer on failure.
   */
  parseType(): Type {
    const token = this.lexer.next();

    // Assign primary types here and we will parse extensions on those types at
    // the end of this function. Return non-primary types.
    let primaryType: Type | undefined;

    // Parse `FunctionType`, `UnitType`, `TupleType`, and `WrappedType`.
    if (token.type === TokenType.Glyph && token.glyph === Glyph.ParenLeft) {
      // Parse a list of types inside parentheses.
      const start = token.loc;
      const types = this.parseCommaList(() => {
        const type = this.parseType();
        if (type.type === TypeType.Error) return undefined;
        return type;
      }, Glyph.ParenRight);
      const end = this.lexer.next().loc;
      const nextToken = this.lexer.peek();

      // Parse `FunctionType`. Notably we return since functions are not
      // a `PrimaryType`!
      if (
        nextToken.type === TokenType.Glyph &&
        nextToken.glyph === Glyph.Arrow
      ) {
        this.lexer.next();
        const body = this.parseType();
        return FunctionType(start.between(body.loc), types, body);
      }

      // Finish parsing either `UnitType`, `TupleType`, or `WrappedType`.
      const loc = start.between(end);
      primaryType = createParenListType(loc, types);
    }

    // Parse `ReferenceType`.
    if (
      token.type === TokenType.Identifier &&
      !Identifier.getBindingKeyword(token.identifier)
    ) {
      const start = token.loc;
      const nextToken = this.lexer.peek();

      // Parse `FunctionType`. Notably we return since functions are not
      // a `PrimaryType`!
      if (
        nextToken.type === TokenType.Glyph &&
        nextToken.glyph === Glyph.Arrow
      ) {
        this.lexer.next();
        const body = this.parseType();
        return FunctionType(
          start.between(body.loc),
          [ReferenceType(token.loc, token.identifier)],
          body
        );
      }

      primaryType = ReferenceType(token.loc, token.identifier);
    }

    // Parse `RecordType`.
    if (token.type === TokenType.Glyph && token.glyph === Glyph.BraceLeft) {
      const start = token.loc;
      const properties = this.parseCommaList(() => {
        const key = this.parseIdentifier();
        if (key === undefined) return undefined;
        const optional = this.tryParseGlyph(Glyph.Question);
        this.parseGlyph(Glyph.Colon);
        const value = this.parseType();
        return optional
          ? RecordTypeProperty.optional(Name(key.loc, key.identifier), value)
          : RecordTypeProperty(Name(key.loc, key.identifier), value);
      }, Glyph.BraceRight);
      const end = this.lexer.next().loc;
      const loc = start.between(end);
      primaryType = RecordType(loc, properties);
    }

    // Parse `QuanitifedType`
    if (token.type === TokenType.Glyph && token.glyph === Glyph.LessThan) {
      const start = token.loc;
      const typeParameters = this.parseCommaList(
        () => this.parseGenericParameter(),
        Glyph.GreaterThan
      );
      this.lexer.next();
      const body = this.parseType();
      return QuantifiedType(start.between(body.loc), typeParameters, body);
    }

    // Return an error if we could not parse a primary type.
    if (primaryType === undefined) {
      const error = UnexpectedTokenError(token, ExpectedType);
      this.errors.push(error);
      return ErrorType(token.loc, error);
    }

    return this.parsePrimaryTypeExtension(primaryType);
  }

  /**
   * Parses the extensions to a balanced primary type.
   */
  parsePrimaryTypeExtension(primaryType: Type): Type {
    let type = primaryType;

    while (true) {
      const token = this.lexer.peek();

      // Parse `MemberType`.
      if (token.type === TokenType.Glyph && token.glyph === Glyph.Dot) {
        this.lexer.next();
        const identifier = this.parseIdentifier();
        if (identifier === undefined) return type;
        type = MemberType(
          type.loc.between(identifier.loc),
          type,
          Name(identifier.loc, identifier.identifier)
        );
        continue;
      }

      // Parse `GenericType`
      if (token.type === TokenType.Glyph && token.glyph === Glyph.LessThan) {
        this.lexer.next();
        const types = this.parseCommaList(() => {
          const type = this.parseType();
          if (type.type === TypeType.Error) return undefined;
          return type;
        }, Glyph.GreaterThan);
        const end = this.lexer.next().loc;
        type = GenericType(type.loc.between(end), type, types);
        continue;
      }

      break;
    }

    return type;
  }

  /**
   * Parses the `GenericPattern` grammar. Returns `undefined` if parsing fails.
   */
  parseGenericParameter(): TypeParameter | undefined {
    const identifier = this.parseIdentifier();
    if (identifier === undefined) return undefined;
    return TypeParameter(Name(identifier.loc, identifier.identifier), []);
  }

  /**
   * Parses a list seperated by commas. Supports trailing commas but requires an
   * ending to look for to do so.
   *
   * - If `parseItem()` returns `undefined` then we don’t add it to the final
   *   items array and we don’t try to parse a comma.
   * - If we try to parse a comma but there is none then we report an error and
   *   try to parse the next item.
   *
   * Takes care to not enter an infinite loop if `parseItem()` does not advance
   * the lexer.
   */
  parseCommaList<T>(parseItem: () => T | undefined, endGlyph: Glyph): Array<T> {
    // There’s a fair bit of subtle logic in this function to deal with trailing
    // commas and error recovery. If done wrong we could enter an infinite loop
    // or get the wrong behavior. It is important we get this right.

    // We will put all the items we parse in this array. Any items that fail to
    // parse will not be reflected in this array.
    const items = [];

    // At the beginning of our loop we check whether or not we have reached the
    // end of our comma list. This check depends on a peek of the next token.
    // So at the end of every iteration of the loop we must assign a peek of the
    // next token for that check to work correctly.
    let token = this.lexer.peek();

    while (true) {
      // The aforementioned check to see if we have reached the end of our
      // comma list.
      if (token.type === TokenType.Glyph && token.glyph === endGlyph) break;

      // Try to parse an item. However, not all items will successfully parse.
      // These items will not be added to our items list.
      const item = parseItem();

      if (item !== undefined) {
        // If we successfully parsed an item then add it to our list.
        items.push(item);

        // Peek the next token so we can perform our list end check again. If we
        // don’t have a trailing comma then our list ends after the last item.
        token = this.lexer.peek();
        if (token.type === TokenType.Glyph && token.glyph === endGlyph) break;

        // Try to parse a comma. If it fails we don’t consume any tokens in case
        // the programmer forgot to write a comma.
        this.parseGlyph(Glyph.Comma);

        // Peek the next token for our check at the beginning of the loop.
        // Otherwise we will be using outdated information!
        token = this.lexer.peek();
      } else {
        // Ok, so here we failed to parse an item. Which means we are in error
        // recovery mode. Peek the next token for our check at the beginning of
        // the loop. Also store the last token in a temporary variable. More on
        // that next.
        const lastToken = token;
        token = this.lexer.peek();

        // There’s a risk for an infinite loop here! If `parseItem()` does not
        // consume any tokens then we will repeat the loop with the same token
        // that we already know is a bad token. So if the token location did not
        // change between our last peek and the one above then lets advance the
        // lexer so we don’t get stuck trying to parse the same token forever.
        if (lastToken.loc.start.equals(token.loc.start)) {
          // If we’ve reached the end of our token stream then don’t even try
          // to advance the lexer since we’ll keep getting the end
          // token forever.
          if (token.type === TokenType.End) break;

          this.lexer.next();
          token = this.lexer.peek();
        }
      }
    }

    // We’ve reached the end of our comma list! Return the items we’ve parsed.
    return items;
  }

  /**
   * Test parser for `parseCommaList()`.
   */
  parseCommaListTest(): Array<string> {
    if (!this.parseGlyph(Glyph.ParenLeft)) return [];
    const identifiers = this.parseCommaList<Identifier>(() => {
      const identifier = this.parseIdentifier();
      return identifier ? identifier.identifier : undefined;
    }, Glyph.ParenRight);
    this.lexer.next();
    return identifiers;
  }

  /**
   * Parses a single glyph.
   *
   * - If the next token is the provided glyph then we consume it and
   *   return true.
   * - If the next token is not the provided glyph then we report an error and
   *   return false. We do not advance the lexer!
   */
  parseGlyph(glyph: Glyph): boolean {
    const token = this.lexer.peek();
    if (token.type === TokenType.Glyph && token.glyph === glyph) {
      this.lexer.next();
      return true;
    }
    this.errors.push(UnexpectedTokenError(token, ExpectedGlyph(glyph)));
    return false;
  }

  /**
   * Tries to parse a glyph. Returns true if we could parse it. Returns false if
   * we could not.
   */
  tryParseGlyph(glyph: Glyph): boolean {
    const token = this.lexer.peek();
    if (token.type === TokenType.Glyph && token.glyph === glyph) {
      this.lexer.next();
      return true;
    }
    return false;
  }

  /**
   * Parses a single identifier.
   *
   * - If the next token is an identifier then we consume it and return true.
   * - If the next token is not an identifier then don’t consume a token, report
   *   an error, and return false.
   */
  parseIdentifier(): IdentifierToken | undefined {
    const token = this.lexer.peek();
    if (token.type === TokenType.Identifier) {
      this.lexer.next();
      return token;
    }
    this.errors.push(UnexpectedTokenError(token, ExpectedIdentifier));
    return undefined;
  }

  /**
   * Parses the ending of our token stream.
   *
   * - If the next token is the ending then return true.
   * - If the next token is not the ending then we report an error and
   *   return false.
   */
  parseEnding(): boolean {
    const token = this.lexer.next();
    if (token.type === TokenType.End) return true;
    this.errors.push(UnexpectedTokenError(token, ExpectedEnd));
    return false;
  }
}

function createParenListType(loc: Loc, types: ReadonlyArray<Type>): Type {
  if (types.length === 0) {
    return UnitType(loc);
  } else if (types.length === 1) {
    return WrappedType(loc, types[0]);
  } else {
    return TupleType(loc, ReadonlyArray2.create(types));
  }
}
