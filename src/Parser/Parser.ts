import {Array1, Array2} from '../Utils/ArrayN';
import {Err, Ok, Result} from '../Utils/Result';

import {
  AliasPattern,
  BindingName,
  BindingPattern,
  DeconstructPattern,
  Expression,
  FunctionType,
  GenericType,
  HoleExpression,
  HolePattern,
  ListExpression,
  ListPattern,
  MemberType,
  Name,
  Pattern,
  QualifiedPattern,
  QuantifiedType,
  RecordExpression,
  RecordExpressionProperty,
  RecordPattern,
  RecordPatternProperty,
  RecordType,
  RecordTypeProperty,
  ReferenceExpression,
  ReferenceType,
  TupleExpression,
  TupleExpressionElement,
  TuplePattern,
  TuplePatternElement,
  TupleType,
  Type,
  TypeParameter,
  UnitExpression,
  UnitPattern,
  UnitType,
  WrappedExpression,
  WrappedPattern,
  WrappedType,
} from './Ast';
import {
  ExpectedBindingIdentifier,
  ExpectedEnd,
  ExpectedExpression,
  ExpectedGlyph,
  ExpectedIdentifier,
  ExpectedPattern,
  ExpectedType,
  ParserError,
  UnexpectedTokenError,
} from './Error';
import {BindingIdentifier, Keyword} from './Identifier';
import {Glyph, IdentifierToken, Lexer, TokenType} from './Lexer';
import {Loc, Pos} from './Loc';

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

export function parseExpression(lexer: Lexer): Result<Expression, ParserError> {
  try {
    const parser = new Parser(lexer);
    const pattern = parser.parseExpression();
    parser.parseEnding();
    return Ok(pattern);
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
   * Parses the `Type` grammar.
   */
  parseType(): Type {
    const token = this.lexer.next();

    // Assign primary types here and we will parse extensions on those types at
    // the end of this function. Return non-primary types.
    let primaryType: Type | undefined;

    // Parse `FunctionType`, `UnitType`, `TupleType`, and `WrappedType`.
    if (token.type === TokenType.Glyph && token.glyph === Glyph.ParenLeft) {
      // Parse a list of types inside parentheses.
      const start = token.loc.start;
      const types = this.parseCommaList(
        () => this.parseType(),
        Glyph.ParenRight
      );
      const end = this.lexer.next().loc.end;
      const nextToken = this.lexer.peek();

      // Parse `FunctionType`. Notably we return since functions are not
      // a `PrimaryType`!
      if (
        nextToken.type === TokenType.Glyph &&
        nextToken.glyph === Glyph.Arrow
      ) {
        this.lexer.next();
        const body = this.parseType();
        return FunctionType(new Loc(start, body.loc.end), types, body);
      }

      // Finish parsing either `UnitType`, `TupleType`, or `WrappedType`.
      const loc = new Loc(start, end);
      primaryType = createParenListType(loc, types);
    }

    // Parse `ReferenceType`.
    if (token.type === TokenType.Identifier) {
      const identifier = BindingIdentifier.create(token.identifier);
      if (identifier !== undefined) {
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
            new Loc(token.loc.start, body.loc.end),
            [ReferenceType(token.loc, identifier)],
            body
          );
        }

        primaryType = ReferenceType(token.loc, identifier);
      }
    }

    // Parse `RecordType`.
    if (token.type === TokenType.Glyph && token.glyph === Glyph.BraceLeft) {
      const start = token.loc.start;
      const properties = this.parseCommaList(() => {
        const key = this.parseName();
        const optional = this.tryParseGlyph(Glyph.Question);
        this.parseGlyph(Glyph.Colon);
        const value = this.parseType();
        return RecordTypeProperty(key, value, {optional});
      }, Glyph.BraceRight);
      const end = this.lexer.next().loc.end;
      const loc = new Loc(start, end);
      primaryType = RecordType(loc, properties);
    }

    // Parse `QuantifiedType`
    if (token.type === TokenType.Glyph && token.glyph === Glyph.LessThan) {
      const start = token.loc.start;
      const typeParameters = this.parseCommaList(
        () => this.parseGenericParameter(),
        Glyph.GreaterThan
      );
      this.lexer.next();
      const body = this.parseType();
      return QuantifiedType(new Loc(start, body.loc.end), typeParameters, body);
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
        const name = this.parseName();
        const loc = new Loc(type.loc.start, name.loc.end);
        type = MemberType(loc, type, name);
        continue;
      }

      // Parse `GenericType`
      if (token.type === TokenType.Glyph && token.glyph === Glyph.LessThan) {
        this.lexer.next();
        const types = this.parseCommaList(
          () => this.parseType(),
          Glyph.GreaterThan
        );
        const end = this.lexer.next().loc.end;
        type = GenericType(new Loc(type.loc.start, end), type, types);
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
    const name = this.parseName();

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

    return TypeParameter(name, typeParameters);
  }

  /**
   * Parses the `Expression` grammar.
   */
  parseExpression(): Expression {
    const token = this.lexer.next();

    // Parse `ReferenceExpression`.
    if (token.type === TokenType.Identifier) {
      const identifier = BindingIdentifier.create(token.identifier);
      if (identifier !== undefined) {
        return ReferenceExpression(token.loc, identifier);
      }
    }

    // Parse `BindingPatternHole`
    if (
      token.type === TokenType.Keyword &&
      token.keyword === Keyword.Underscore
    ) {
      return HoleExpression(token.loc);
    }

    // Parse `UnitExpression`, `TupleExpression`, and `WrappedExpression`.
    if (token.type === TokenType.Glyph && token.glyph === Glyph.ParenLeft) {
      const start = token.loc.start;
      const elements = this.parseCommaList(() => {
        const expression = this.parseExpression();
        const type = this.tryParseGlyph(Glyph.Colon)
          ? this.parseType()
          : undefined;
        return TupleExpressionElement(expression, type);
      }, Glyph.ParenRight);
      const end = this.lexer.next().loc.end;
      const loc = new Loc(start, end);
      if (elements.length === 0) {
        return UnitExpression(loc);
      } else if (elements.length === 1) {
        const element = elements[0];
        return WrappedExpression(loc, element.expression, element.type);
      } else {
        return TupleExpression(loc, Array2.create(elements));
      }
    }

    // Parse `RecordExpression`.
    if (token.type === TokenType.Glyph && token.glyph === Glyph.BraceLeft) {
      const start = token.loc.start;
      let extension: Expression | undefined;

      // We may need to parse an extension, so peek our next token to tell
      // whether or not that is the case.
      const token2 = this.lexer.peek();

      // If we immediately see a closing brace (`}`) then we know we have an
      // empty record so end early by returning that empty record.
      if (
        token2.type === TokenType.Glyph &&
        token2.glyph === Glyph.BraceRight
      ) {
        const end = this.lexer.next().loc.end;
        return RecordExpression(new Loc(start, end), undefined, []);
      }

      // If we have an `Identifier` then we might either have an extension
      // or we might have a record property key.
      if (token2.type === TokenType.Identifier) {
        const token3 = this.lexer.peek2();
        // We know that if we have a property key then it must be followed by
        // one of the tokens: `=`, `,`, `}`, `:`, or `?`.
        const isPropertyKey =
          token3.type === TokenType.Glyph &&
          (token3.glyph === Glyph.Equals ||
            token3.glyph === Glyph.Comma ||
            token3.glyph === Glyph.BraceRight ||
            token3.glyph === Glyph.Colon ||
            token3.glyph === Glyph.Question);
        // If we don’t have a property key as determined by `token3` then we
        // have an extension.
        if (!isPropertyKey) {
          extension = this.parseExpression();
          this.parseGlyph(Glyph.Bar);
        }
      } else {
        // If the next token is not an identifier then we know we have
        // an extension.
        extension = this.parseExpression();
        this.parseGlyph(Glyph.Bar);
      }

      // Parse all of the record properties in a comma list.
      const properties = this.parseCommaList(() => {
        // Parse the name of this record property. If we used punned record
        // initialization syntax then we will later assert that this name is a
        // binding identifier.
        const key = this.parseName();

        // The code to parse a type annotation here is kind of interesting.
        // Currently optional properties must have a type annotation so first we
        // try to parse a question mark. If we parse a question mark then we
        // require a colon and a type to be parsed next. If there is no question
        // mark then we may still optionally want to parse a type annotation.
        const optional = this.tryParseGlyph(Glyph.Question);
        if (optional) this.parseGlyph(Glyph.Colon);
        const type =
          optional || this.tryParseGlyph(Glyph.Colon)
            ? this.parseType()
            : undefined;

        // Parse the value initializer for this expression. If we are using the
        // syntax where we don’t have an initializer (e.g. `{ a, b }`) then we
        // need to go back and throw an error if our key name is not a
        // binding identifier.
        let value: Expression;
        if (this.tryParseGlyph(Glyph.Equals)) {
          value = this.parseExpression();
        } else {
          const identifier = BindingIdentifier.create(key.identifier);
          if (identifier === undefined) {
            throw UnexpectedTokenError(
              IdentifierToken(key.loc, key.identifier),
              ExpectedBindingIdentifier
            );
          }
          value = ReferenceExpression(key.loc, identifier);
        }

        // Create the record property.
        return RecordExpressionProperty(key, value, type, {optional});
      }, Glyph.BraceRight);

      const end = this.lexer.next().loc.end;
      const loc = new Loc(start, end);
      return RecordExpression(loc, extension, properties);
    }

    // Parse `ListExpression`.
    if (token.type === TokenType.Glyph && token.glyph === Glyph.BracketLeft) {
      const start = token.loc.start;
      const items = this.parseCommaList(
        () => this.parseExpression(),
        Glyph.BracketRight
      );
      const end = this.lexer.next().loc.end;
      const loc = new Loc(start, end);
      return ListExpression(loc, items);
    }

    throw UnexpectedTokenError(token, ExpectedExpression);
  }

  /**
   * Parses the `Pattern` grammar.
   */
  parsePattern(): Pattern {
    const token = this.lexer.next();

    // Parse `BindingPattern`, `QualifiedPattern`, `DeconstructPattern`,
    // and `AliasPattern`.
    if (token.type === TokenType.Identifier) {
      // If the first identifier is not a `BindingIdentifier` then we may not
      // continue parsing any of our grammars.
      const firstIdentifier = BindingIdentifier.create(token.identifier);
      if (firstIdentifier === undefined) {
        throw UnexpectedTokenError(token, ExpectedPattern);
      }

      // If there is an `is` keyword on the same line as our binding identifier
      // then we have an `AliasPattern`.
      if (this.tryParseInformalKeywordOnSameLine(token.loc.end, 'is')) {
        const pattern = this.parsePattern();
        const loc = new Loc(token.loc.start, pattern.loc.end);
        return AliasPattern(
          loc,
          BindingName(token.loc, firstIdentifier),
          pattern
        );
      }

      // If there is a dot following our identifier then we have a path to some
      // namespaced location.
      const identifiers = [Name(token.loc, token.identifier)];
      while (true) {
        if (!this.tryParseGlyph(Glyph.Dot)) break;
        const token = this.parseIdentifier();
        identifiers.push(Name(token.loc, token.identifier));
      }

      // If there is an opening parentheses on the same line as our identifier
      // path then we have a `DeconstructPattern`.
      const lastPos = identifiers[identifiers.length - 1].loc.end;
      if (this.tryParseGlyphOnSameLine(lastPos, Glyph.ParenLeft)) {
        const callee = Array1.create(identifiers);
        const args = this.parseCommaList(
          () => this.parsePattern(),
          Glyph.ParenRight
        );
        const end = this.lexer.next().loc.end;
        const loc = new Loc(token.loc.start, end);
        return DeconstructPattern(loc, callee, args);
      }

      // If we don’t have an open parentheses then have either a
      // `BindingPattern` or a `QualifiedPattern`.
      if (identifiers.length === 1) {
        return BindingPattern(token.loc, firstIdentifier);
      } else {
        const start = identifiers[0].loc.start;
        const end = identifiers[identifiers.length - 1].loc.end;
        const loc = new Loc(start, end);
        return QualifiedPattern(loc, Array2.create(identifiers));
      }
    }

    // Parse `BindingPattern` hole.
    if (
      token.type === TokenType.Keyword &&
      token.keyword === Keyword.Underscore
    ) {
      return HolePattern(token.loc);
    }

    // Parse `UnitPattern`, `TuplePattern`, and `WrappedPattern`.
    if (token.type === TokenType.Glyph && token.glyph === Glyph.ParenLeft) {
      const start = token.loc.start;
      const elements = this.parseCommaList(() => {
        const pattern = this.parsePattern();
        const type = this.tryParseGlyph(Glyph.Colon)
          ? this.parseType()
          : undefined;
        return TuplePatternElement(pattern, type);
      }, Glyph.ParenRight);
      const end = this.lexer.next().loc.end;
      const loc = new Loc(start, end);
      if (elements.length === 0) {
        return UnitPattern(loc);
      } else if (elements.length === 1) {
        const element = elements[0];
        return WrappedPattern(loc, element.pattern, element.type);
      } else {
        return TuplePattern(loc, Array2.create(elements));
      }
    }

    // Parse `RecordPattern`.
    if (token.type === TokenType.Glyph && token.glyph === Glyph.BraceLeft) {
      const start = token.loc.start;

      // Parse all of the record properties in a comma list.
      const properties = this.parseCommaList(() => {
        // Parse the name of this record property. If we used punned record
        // initialization syntax then we will later assert that this name is a
        // binding identifier.
        const key = this.parseName();

        // The code to parse a type annotation here is kind of interesting.
        // Currently optional properties must have a type annotation so first we
        // try to parse a question mark. If we parse a question mark then we
        // require a colon and a type to be parsed next. If there is no question
        // mark then we may still optionally want to parse a type annotation.
        const optional = this.tryParseGlyph(Glyph.Question);
        if (optional) this.parseGlyph(Glyph.Colon);
        const type =
          optional || this.tryParseGlyph(Glyph.Colon)
            ? this.parseType()
            : undefined;

        // Parse the value initializer for this pattern. If we are using the
        // syntax where we don’t have an initializer (e.g. `{ a, b }`) then we
        // need to go back and throw an error if our key name is not a
        // binding identifier.
        let value: Pattern;
        if (this.tryParseGlyph(Glyph.Equals)) {
          value = this.parsePattern();
        } else {
          const identifier = BindingIdentifier.create(key.identifier);
          if (identifier === undefined) {
            throw UnexpectedTokenError(
              IdentifierToken(key.loc, key.identifier),
              ExpectedBindingIdentifier
            );
          }
          value = BindingPattern(key.loc, identifier);
        }

        // Create the record property.
        return RecordPatternProperty(key, value, type, {optional});
      }, Glyph.BraceRight);

      const end = this.lexer.next().loc.end;
      const loc = new Loc(start, end);
      return RecordPattern(loc, properties);
    }

    // Parse `ListPattern`.
    if (token.type === TokenType.Glyph && token.glyph === Glyph.BracketLeft) {
      const start = token.loc.start;
      const items = this.parseCommaList(
        () => this.parsePattern(),
        Glyph.BracketRight
      );
      const end = this.lexer.next().loc.end;
      const loc = new Loc(start, end);
      return ListPattern(loc, items);
    }

    throw UnexpectedTokenError(token, ExpectedPattern);
  }

  /**
   * Parses a list separated by commas and pushes items into the array parameter
   * supplied. Supports trailing commas but requires an ending to look for to
   * do so.
   *
   * - If `parseItem()` returns `undefined` then we don’t add it to the final
   *   items array and we don’t try to parse a comma.
   * - If we try to parse a comma but there is none then we report an error and
   *   try to parse the next item.
   */
  parseCommaList<T>(parseItem: () => T, endGlyph: Glyph): Array<T> {
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
  parseGlyph(glyph: Glyph): Loc {
    const token = this.lexer.peek();
    if (token.type === TokenType.Glyph && token.glyph === glyph) {
      return this.lexer.next().loc;
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
   * Tries to parse a glyph but only if it is on the same line as the provided
   * position. Returns true if we could parse it. Returns false if we could not.
   */
  tryParseGlyphOnSameLine(pos: Pos, glyph: Glyph): boolean {
    const token = this.lexer.peek();
    if (
      token.type === TokenType.Glyph &&
      token.glyph === glyph &&
      token.loc.start.line === pos.line
    ) {
      this.lexer.next();
      return true;
    }
    return false;
  }

  /**
   * Tries to parse an identifier used as an informal keyword on the same line
   * as the provided position. Returns true if we could parse it. Returns false
   * if we could not.
   */
  tryParseInformalKeywordOnSameLine(pos: Pos, identifier: string): boolean {
    const token = this.lexer.peek();
    if (
      token.type === TokenType.Identifier &&
      token.identifier === identifier &&
      token.loc.start.line === pos.line
    ) {
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
   * Parses a `Name`.
   */
  parseName(): Name {
    const identifier = this.parseIdentifier();
    return Name(identifier.loc, identifier.identifier);
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

function createParenListType(loc: Loc, types: Array<Type>): Type {
  if (types.length === 0) {
    return UnitType(loc);
  } else if (types.length === 1) {
    return WrappedType(loc, types[0]);
  } else {
    return TupleType(loc, Array2.create(types));
  }
}
