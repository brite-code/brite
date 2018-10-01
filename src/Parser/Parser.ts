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
  MatchCase,
  MatchExpression,
  MemberExpression,
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
  ExpectedLineSeparator,
  ExpectedPattern,
  ExpectedType,
  ParserError,
  UnexpectedTokenError,
} from './Error';
import {BindingIdentifier, Keyword} from './Identifier';
import {Glyph, IdentifierToken, Lexer, Token, TokenType} from './Lexer';
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

export function parseLineSeparatorListTest(
  lexer: Lexer
): Result<ReadonlyArray<string>, ParserError> {
  try {
    const parser = new Parser(lexer);
    const result = parser.parseLineSeparatorListTest();
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
  /**
   * Prefixed with an underscore as syntax vinegar. You should not be calling
   * `this._lexer.next()` directly. Instead call `this.nextToken()`.
   */
  private readonly _lexer: Lexer;

  /**
   * The current location of our lexer.
   */
  private currentLoc = Loc.pos(1, 1);

  constructor(lexer: Lexer) {
    this._lexer = lexer;
  }

  /**
   * Advances the lexer to the next token.
   */
  nextToken(): Token {
    const token = this._lexer.next();
    this.currentLoc = token.loc;
    return token;
  }

  /**
   * Looks ahead at the next token.
   */
  peekToken(): Token {
    return this._lexer.peek();
  }

  /**
   * Looks ahead two tokens.
   */
  peek2Token(): Token {
    return this._lexer.peek2();
  }

  /**
   * Parses the `Type` grammar.
   */
  parseType(): Type {
    const token = this.nextToken();

    // Assign primary types here and we will parse extensions on those types at
    // the end of this function. Return non-primary types.
    let primaryType: Type;

    if (
      // Parse `FunctionType`, `UnitType`, `TupleType`, and `WrappedType`.
      token.type === TokenType.Glyph &&
      token.glyph === Glyph.ParenLeft
    ) {
      // Parse a list of types inside parentheses.
      const start = token.loc.start;
      const types = this.parseCommaList(
        () => this.parseType(),
        Glyph.ParenRight
      );
      const end = this.nextToken().loc.end;
      const nextToken = this.peekToken();

      // Parse `FunctionType`. Notably we return since functions are not
      // a `PrimaryType`!
      if (
        nextToken.type === TokenType.Glyph &&
        nextToken.glyph === Glyph.Arrow
      ) {
        this.nextToken();
        const body = this.parseType();
        return FunctionType(new Loc(start, body.loc.end), types, body);
      }

      // Finish parsing either `UnitType`, `TupleType`, or `WrappedType`.
      const loc = new Loc(start, end);
      primaryType = createParenListType(loc, types);
    } else if (
      // Parse `ReferenceType`.
      token.type === TokenType.Identifier
    ) {
      const identifier = BindingIdentifier.create(token.identifier);
      if (identifier !== undefined) {
        const nextToken = this.peekToken();

        // Parse `FunctionType`. Notably we return since functions are not
        // a `PrimaryType`!
        if (
          nextToken.type === TokenType.Glyph &&
          nextToken.glyph === Glyph.Arrow
        ) {
          this.nextToken();
          const body = this.parseType();
          return FunctionType(
            new Loc(token.loc.start, body.loc.end),
            [ReferenceType(token.loc, identifier)],
            body
          );
        }

        primaryType = ReferenceType(token.loc, identifier);
      } else {
        throw UnexpectedTokenError(token, ExpectedType);
      }
    } else if (
      // Parse `RecordType`.
      token.type === TokenType.Glyph &&
      token.glyph === Glyph.BraceLeft
    ) {
      const start = token.loc.start;
      const properties = this.parseCommaList(() => {
        const key = this.parseName();
        const optional = this.tryParseGlyph(Glyph.Question);
        this.parseGlyph(Glyph.Colon);
        const value = this.parseType();
        return RecordTypeProperty(key, value, {optional});
      }, Glyph.BraceRight);
      const end = this.nextToken().loc.end;
      const loc = new Loc(start, end);
      primaryType = RecordType(loc, properties);
    } else if (
      // Parse `QuantifiedType`.
      token.type === TokenType.Glyph &&
      token.glyph === Glyph.LessThan
    ) {
      const start = token.loc.start;
      const typeParameters = this.parseCommaList(
        () => this.parseGenericParameter(),
        Glyph.GreaterThan
      );
      this.nextToken();
      const body = this.parseType();
      return QuantifiedType(new Loc(start, body.loc.end), typeParameters, body);
    } else {
      throw UnexpectedTokenError(token, ExpectedType);
    }

    // Parse extensions to the `PrimaryType` grammar.
    while (true) {
      const token = this.peekToken();

      // Parse `MemberType`.
      if (token.type === TokenType.Glyph && token.glyph === Glyph.Dot) {
        this.nextToken();
        const name = this.parseName();
        const loc = new Loc(primaryType.loc.start, name.loc.end);
        primaryType = MemberType(loc, primaryType, name);
        continue;
      }

      // Parse `GenericType`
      if (token.type === TokenType.Glyph && token.glyph === Glyph.LessThan) {
        this.nextToken();
        const types = this.parseCommaList(
          () => this.parseType(),
          Glyph.GreaterThan
        );
        const end = this.nextToken().loc.end;
        const loc = new Loc(primaryType.loc.start, end);
        primaryType = GenericType(loc, primaryType, types);
        continue;
      }

      break;
    }

    return primaryType;
  }

  /**
   * Parses the `GenericPattern` grammar. Returns `undefined` if parsing fails.
   */
  parseGenericParameter(): TypeParameter {
    const name = this.parseName();

    // There may be some optional bounds on the generic parameter after a colon.
    // If we see a colon then try and parse our type bounds.
    const typeParameters: Array<Type> = [];
    const token = this.peekToken();
    if (token.type === TokenType.Glyph && token.glyph === Glyph.Colon) {
      this.nextToken();
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
    const token = this.nextToken();

    let primaryExpression: Expression;

    if (
      // Parse `ReferenceExpression`.
      token.type === TokenType.Identifier
    ) {
      const identifier = BindingIdentifier.create(token.identifier);
      if (identifier !== undefined) {
        primaryExpression = ReferenceExpression(token.loc, identifier);
      } else if (
        // Parse `MatchExpression`.
        token.identifier === 'match'
      ) {
        const start = token.loc.start;
        const test = this.parseExpression();
        this.parseGlyph(Glyph.Colon);
        this.parseGlyph(Glyph.ParenLeft);
        const cases = this.parseLineSeparatorList(() => {
          const binding = this.parsePattern();
          this.parseGlyph(Glyph.Arrow);
          const body = this.parseExpression();
          return MatchCase([binding], undefined, body);
        }, Glyph.ParenRight);
        const end = this.nextToken().loc.end;
        const loc = new Loc(start, end);
        return MatchExpression(loc, test, cases);
      } else {
        throw UnexpectedTokenError(token, ExpectedExpression);
      }
    } else if (
      // Parse `BindingPatternHole`
      token.type === TokenType.Keyword &&
      token.keyword === Keyword.Underscore
    ) {
      primaryExpression = HoleExpression(token.loc);
    } else if (
      // Parse `UnitExpression`, `TupleExpression`, and `WrappedExpression`.
      token.type === TokenType.Glyph &&
      token.glyph === Glyph.ParenLeft
    ) {
      const start = token.loc.start;

      // Parse a list of tuple elements with optional annotations.
      const elements = this.parseCommaList(() => {
        const expression = this.parseExpression();
        const type = this.tryParseGlyph(Glyph.Colon)
          ? this.parseType()
          : undefined;
        return TupleExpressionElement(expression, type);
      }, Glyph.ParenRight);

      const end = this.nextToken().loc.end;
      const loc = new Loc(start, end);

      // Turn our list of elements into the appropriate expression node. If
      // there were no elements then we have a unit expression. If there was one
      // element then we have a simple wrapped expression. If there were many
      // elements then we have a tuple expression.
      if (elements.length === 0) {
        primaryExpression = UnitExpression(loc);
      } else if (elements.length === 1) {
        const element = elements[0];
        primaryExpression = WrappedExpression(
          loc,
          element.expression,
          element.type
        );
      } else {
        primaryExpression = TupleExpression(loc, Array2.create(elements));
      }
    } else if (
      // Parse `RecordExpression`.
      token.type === TokenType.Glyph &&
      token.glyph === Glyph.BraceLeft
    ) {
      const start = token.loc.start;
      let extension: Expression | undefined;

      // We may need to parse an extension, so peek our next token to tell
      // whether or not that is the case.
      const token2 = this.peekToken();

      // If we immediately see a closing brace (`}`) then we know we have an
      // empty record so end early by returning that empty record.
      if (
        token2.type === TokenType.Glyph &&
        token2.glyph === Glyph.BraceRight
      ) {
        const end = this.nextToken().loc.end;
        primaryExpression = RecordExpression(
          new Loc(start, end),
          undefined,
          []
        );
      } else {
        // If we have an `Identifier` then we might either have an extension
        // or we might have a record property key.
        if (token2.type === TokenType.Identifier) {
          const token3 = this.peek2Token();
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
          // Currently optional properties must have a type annotation so first
          // we try to parse a question mark. If we parse a question mark then
          // we require a colon and a type to be parsed next. If there is no
          // question mark then we may still optionally want to parse a
          // type annotation.
          const optional = this.tryParseGlyph(Glyph.Question);
          if (optional) this.parseGlyph(Glyph.Colon);
          const type =
            optional || this.tryParseGlyph(Glyph.Colon)
              ? this.parseType()
              : undefined;

          // Parse the value initializer for this expression. If we are using
          // the syntax where we don’t have an initializer (e.g. `{ a, b }`)
          // then we need to go back and throw an error if our key name is not a
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

        const end = this.nextToken().loc.end;
        const loc = new Loc(start, end);
        primaryExpression = RecordExpression(loc, extension, properties);
      }
    } else if (
      // Parse `ListExpression`.
      token.type === TokenType.Glyph &&
      token.glyph === Glyph.BracketLeft
    ) {
      const start = token.loc.start;
      const items = this.parseCommaList(
        () => this.parseExpression(),
        Glyph.BracketRight
      );
      const end = this.nextToken().loc.end;
      const loc = new Loc(start, end);
      primaryExpression = ListExpression(loc, items);
    } else {
      throw UnexpectedTokenError(token, ExpectedExpression);
    }

    // Parse extensions to the `PrimaryExpression` grammar.
    while (true) {
      const token = this.peekToken();

      // Parse `MemberExpression`.
      if (token.type === TokenType.Glyph && token.glyph === Glyph.Dot) {
        this.nextToken();
        const name = this.parseName();
        const loc = new Loc(primaryExpression.loc.start, name.loc.end);
        primaryExpression = MemberExpression(loc, primaryExpression, name);
        continue;
      }

      break;
    }

    return primaryExpression;
  }

  /**
   * Parses the `Pattern` grammar.
   */
  parsePattern(): Pattern {
    const token = this.nextToken();

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
        const end = this.nextToken().loc.end;
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
      const end = this.nextToken().loc.end;
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

      const end = this.nextToken().loc.end;
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
      const end = this.nextToken().loc.end;
      const loc = new Loc(start, end);
      return ListPattern(loc, items);
    }

    throw UnexpectedTokenError(token, ExpectedPattern);
  }

  /**
   * Parses a list separated by commas and pushes items into the array parameter
   * supplied. Supports trailing commas but requires an ending to look for to
   * do so.
   */
  parseCommaList<T>(parseItem: () => T, endGlyph: Glyph): Array<T> {
    const items: Array<T> = [];

    // At the beginning of our loop we check whether or not we have reached the
    // end of our comma list. This check depends on a peek of the next token.
    // So at the end of every iteration of the loop we must assign a peek of the
    // next token for that check to work correctly.
    let token = this.peekToken();

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
      token = this.peekToken();
      if (token.type === TokenType.Glyph && token.glyph === endGlyph) break;

      // Parse a comma.
      this.parseGlyph(Glyph.Comma);

      // Peek the next token for our check at the beginning of the loop.
      token = this.peekToken();
    }

    // We’ve reached the end of our comma list! Return the items we’ve parsed.
    return items;
  }

  /**
   * Parses a list separated by the `LineSeparator` specification construct. A
   * `LineSeparator` is either a newline or a semicolon. Supports trailing
   * separators but requires an ending glyph to look for to do so.
   */
  parseLineSeparatorList<T>(parseItem: () => T, endGlyph: Glyph): Array<T> {
    const items: Array<T> = [];

    while (true) {
      // If we see the end glyph then break out of our loop. We have this check
      // here both to stop iterating for zero items and to allow
      // trailing `LineSeparator`s.
      const token1 = this.peekToken();
      if (token1.type === TokenType.Glyph && token1.glyph === endGlyph) break;

      // Parse an item and put it in our items list.
      const item = parseItem();
      items.push(item);

      // If we see the end glyph then break out of our loop.
      const token2 = this.peekToken();
      if (token2.type === TokenType.Glyph && token2.glyph === endGlyph) break;

      // If the token is a semicolon then we have a `LineSeparator` so advance
      // and go through another iteration of the loop.
      if (token2.type === TokenType.Glyph && token2.glyph === Glyph.Semicolon) {
        this.nextToken();
        continue;
      }

      // It doesn’t matter what the next token is, if it is on a new line then
      // we have a newline `LineSeparator`! However, make sure we don’t advance.
      // `parseItem()` will advance and eat this token.
      if (this.currentLoc.end.line !== token2.loc.start.line) {
        continue;
      }

      // If we did not see a semicolon or a newline then throw an unexpected
      // token error.
      throw UnexpectedTokenError(token2, ExpectedLineSeparator);
    }

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
    this.nextToken();
    return identifiers;
  }

  /**
   * Test parser for `parseLineSeparatorList()`.
   */
  parseLineSeparatorListTest(): Array<string> {
    this.parseGlyph(Glyph.ParenLeft);
    const identifiers = this.parseLineSeparatorList(
      () => this.parseIdentifier().identifier,
      Glyph.ParenRight
    );
    this.nextToken();
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
    const token = this.peekToken();
    if (token.type === TokenType.Glyph && token.glyph === glyph) {
      return this.nextToken().loc;
    }
    throw UnexpectedTokenError(token, ExpectedGlyph(glyph));
  }

  /**
   * Tries to parse a glyph. Returns true if we could parse it. Returns false if
   * we could not.
   */
  tryParseGlyph(glyph: Glyph): boolean {
    const token = this.peekToken();
    if (token.type === TokenType.Glyph && token.glyph === glyph) {
      this.nextToken();
      return true;
    }
    return false;
  }

  /**
   * Tries to parse a glyph but only if it is on the same line as the provided
   * position. Returns true if we could parse it. Returns false if we could not.
   */
  tryParseGlyphOnSameLine(pos: Pos, glyph: Glyph): boolean {
    const token = this.peekToken();
    if (
      token.type === TokenType.Glyph &&
      token.glyph === glyph &&
      token.loc.start.line === pos.line
    ) {
      this.nextToken();
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
    const token = this.peekToken();
    if (
      token.type === TokenType.Identifier &&
      token.identifier === identifier &&
      token.loc.start.line === pos.line
    ) {
      this.nextToken();
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
    const token = this.peekToken();
    if (token.type === TokenType.Identifier) {
      this.nextToken();
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
    const token = this.nextToken();
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
