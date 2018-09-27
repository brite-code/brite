import {ReadonlyArray2} from '../Utils/ArrayN';
import {Err, Ok, Result} from '../Utils/Result';

import {
  BindingPattern,
  FunctionType,
  GenericType,
  HolePattern,
  MemberType,
  Name,
  Pattern,
  QuantifiedType,
  RecordType,
  RecordTypeProperty,
  ReferenceType,
  TuplePattern,
  TupleType,
  Type,
  TypeParameter,
  UnitPattern,
  UnitType,
  WrappedPattern,
  WrappedType,
} from './Ast';
import {
  ExpectedEnd,
  ExpectedGlyph,
  ExpectedIdentifier,
  ExpectedPattern,
  ExpectedType,
  ParserError,
  UnexpectedTokenError,
} from './Error';
import {BindingIdentifier, Keyword} from './Identifier';
import {Glyph, IdentifierToken, Lexer, TokenType} from './Lexer';
import {Loc} from './Loc';

export function parseType(lexer: Lexer): Result<Type, ParserError> {
  try {
    const parser = new Parser(lexer);
    const type = parser.parseType();
    parser.parseEnding();
    return Ok(type);
  } catch (error) {
    if (error instanceof Error) throw error;
    return Err(error);
  }
}

export function parsePattern(lexer: Lexer): Result<Pattern, ParserError> {
  try {
    const parser = new Parser(lexer);
    const pattern = parser.parsePattern();
    parser.parseEnding();
    return Ok(pattern);
  } catch (error) {
    if (error instanceof Error) throw error;
    return Err(error);
  }
}

export function parseCommaListTest(
  lexer: Lexer
): Result<ReadonlyArray<string>, ParserError> {
  try {
    const parser = new Parser(lexer);
    const result = parser.parseCommaListTest();
    parser.parseEnding();
    return Ok(result);
  } catch (error) {
    if (error instanceof Error) throw error;
    return Err(error);
  }
}

/**
 * A parser with error recovery.
 *
 * We try our best to massage a source string into an AST in the face of parse
 * errors. There is no grand theory for parse error recovery. We just do what
 * feels right.
 *
 * TODO: Parser error recovery. I wanted to build it in the first version, but
 * if I did it would slow me down and I’d just do a bad job at it. If we are
 * going to have parser error recovery it must be: 1) predictable, 2) not noisy
 * on random input. Writing every parsing rule with error recovery in mind is
 * too hard.
 */
class Parser {
  private readonly lexer: Lexer;

  constructor(lexer: Lexer) {
    this.lexer = lexer;
  }

  /**
   * Parses the `Type` grammar. Does not advance the lexer when we cannot parse
   * a type. Always advances otherwise.
   */
  parseType(): Type {
    const token = this.lexer.peek();

    // Assign primary types here and we will parse extensions on those types at
    // the end of this function. Return non-primary types.
    let primaryType: Type | undefined;

    // Parse `FunctionType`, `UnitType`, `TupleType`, and `WrappedType`.
    if (token.type === TokenType.Glyph && token.glyph === Glyph.ParenLeft) {
      // Parse a list of types inside parentheses.
      const start = this.lexer.next().loc;
      const types = this.parseCommaList(
        () => this.parseType(),
        Glyph.ParenRight
      );
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
    if (token.type === TokenType.Identifier) {
      const identifier = BindingIdentifier.create(token.identifier);
      if (identifier !== undefined) {
        this.lexer.next();
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
            token.loc.between(body.loc),
            [ReferenceType(token.loc, identifier)],
            body
          );
        }

        primaryType = ReferenceType(token.loc, identifier);
      }
    }

    // Parse `RecordType`.
    if (token.type === TokenType.Glyph && token.glyph === Glyph.BraceLeft) {
      const start = this.lexer.next().loc;
      const properties = this.parseCommaList(() => {
        const key = this.parseIdentifier();
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

    // Parse `QuantifiedType`
    if (token.type === TokenType.Glyph && token.glyph === Glyph.LessThan) {
      const start = this.lexer.next().loc;
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
      throw UnexpectedTokenError(token, ExpectedType);
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
        const types = this.parseCommaList(
          () => this.parseType(),
          Glyph.GreaterThan
        );
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
  parseGenericParameter(): TypeParameter {
    const identifier = this.parseIdentifier();

    // There may be some optional bounds on the generic parameter after a colon.
    // If we see a colon then try and parse our type bounds.
    const typeParameters: Array<Type> = [];
    const token = this.lexer.peek();
    if (token.type === TokenType.Glyph && token.glyph === Glyph.Colon) {
      this.lexer.next();
      while (true) {
        // Try to parse a type and add it to our `typeParameters` array.
        typeParameters.push(this.parseType());

        // If there is a plus (`+`) glyph then go for another round of the loop.
        // Otherwise stop trying to parse.
        if (!this.tryParseGlyph(Glyph.Plus)) break;
      }
    }

    return TypeParameter(
      Name(identifier.loc, identifier.identifier),
      typeParameters
    );
  }

  /**
   * Parses the `Pattern` grammar. Does not advance the lexer if we fail to
   * parse a pattern.
   */
  parsePattern(): Pattern {
    const token = this.lexer.peek();

    // Parse `BindingPattern` identifier.
    if (token.type === TokenType.Identifier) {
      const identifier = BindingIdentifier.create(token.identifier);
      if (identifier !== undefined) {
        this.lexer.next();
        return BindingPattern(token.loc, identifier);
      }
    }

    // Parse `BindingPattern` hole.
    if (
      token.type === TokenType.Keyword &&
      token.keyword === Keyword.Underscore
    ) {
      this.lexer.next();
      return HolePattern(token.loc);
    }

    // Parse `UnitPattern`, `TuplePattern`, and `WrappedPattern`.
    if (token.type === TokenType.Glyph && token.glyph === Glyph.ParenLeft) {
      const start = this.lexer.next().loc;
      const patterns = this.parseCommaList(
        () => this.parsePattern(),
        Glyph.ParenRight
      );
      const end = this.lexer.next().loc;
      const loc = start.between(end);
      if (patterns.length === 0) {
        return UnitPattern(loc);
      } else if (patterns.length === 1) {
        return WrappedPattern(loc, patterns[0], undefined);
      } else {
        return TuplePattern(loc, ReadonlyArray2.create(patterns));
      }
    }

    throw UnexpectedTokenError(token, ExpectedPattern);
  }

  /**
   * Parses a list separated by commas. Supports trailing commas but requires an
   * ending to look for to do so.
   *
   * - If `parseItem()` returns `undefined` then we don’t add it to the final
   *   items array and we don’t try to parse a comma.
   * - If we try to parse a comma but there is none then we report an error and
   *   try to parse the next item.
   */
  parseCommaList<T>(parseItem: () => T, endGlyph: Glyph): Array<T> {
    // We will put all the items we parse in this array.
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

      // If we successfully parsed an item then add it to our list.
      items.push(item);

      // Peek the next token so we can perform our list end check again. If we
      // don’t have a trailing comma then our list ends after the last item.
      token = this.lexer.peek();
      if (token.type === TokenType.Glyph && token.glyph === endGlyph) break;

      // Parse a comma.
      this.parseGlyph(Glyph.Comma);

      // Peek the next token for our check at the beginning of the loop.
      token = this.lexer.peek();
    }

    // We’ve reached the end of our comma list! Return the items we’ve parsed.
    return items;
  }

  /**
   * Test parser for `parseCommaList()`.
   */
  parseCommaListTest(): Array<string> {
    this.parseGlyph(Glyph.ParenLeft);
    const identifiers = this.parseCommaList(
      () => this.parseIdentifier().identifier,
      Glyph.ParenRight
    );
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
  parseGlyph(glyph: Glyph) {
    const token = this.lexer.peek();
    if (token.type === TokenType.Glyph && token.glyph === glyph) {
      this.lexer.next();
      return;
    }
    throw UnexpectedTokenError(token, ExpectedGlyph(glyph));
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
  parseIdentifier(): IdentifierToken {
    const token = this.lexer.peek();
    if (token.type === TokenType.Identifier) {
      this.lexer.next();
      return token;
    }
    throw UnexpectedTokenError(token, ExpectedIdentifier);
  }

  /**
   * Parses the ending of our token stream.
   *
   * - If the next token is the ending then return true.
   * - If the next token is not the ending then we report an error and
   *   return false.
   */
  parseEnding() {
    const token = this.lexer.next();
    if (token.type === TokenType.End) return;
    throw UnexpectedTokenError(token, ExpectedEnd);
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
