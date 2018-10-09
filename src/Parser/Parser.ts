import {Array1, Array2} from '../Utils/ArrayN';
import {Err, Ok, Result} from '../Utils/Result';

import {
  Access,
  AliasPattern,
  BinaryExpression,
  BinaryExpressionOperator,
  BindingName,
  BindingPattern,
  BindingStatement,
  BlockExpression,
  BreakExpression,
  CallExpression,
  ClassDeclaration,
  ClassMember,
  ClassMethod,
  ConditionalExpression,
  ContinueExpression,
  Declaration,
  DeconstructPattern,
  Expression,
  ExpressionKind,
  ExpressionStatement,
  ForLoopStatement,
  FunctionDeclaration,
  FunctionExpression,
  FunctionParameter,
  FunctionType,
  GenericType,
  HoleExpression,
  HolePattern,
  ListExpression,
  ListPattern,
  LogicalExpression,
  LogicalExpressionOperator,
  LoopExpression,
  MatchCase,
  MatchExpression,
  MemberExpression,
  MemberType,
  Name,
  Pattern,
  PatternExpression,
  QualifiedPattern,
  RecordExpression,
  RecordExpressionProperty,
  RecordPattern,
  RecordPatternProperty,
  RecordType,
  RecordTypeProperty,
  ReferenceExpression,
  ReferenceType,
  ReturnExpression,
  Statement,
  StatementKind,
  TupleExpression,
  TupleExpressionElement,
  TuplePattern,
  TuplePatternElement,
  TupleType,
  Type,
  TypeDeclaration,
  TypeKind,
  TypeParameter,
  UnaryExpression,
  UnaryExpressionOperator,
  UnitExpression,
  UnitPattern,
  UnitType,
  WhileLoopStatement,
  WrappedExpression,
  WrappedPattern,
  WrappedType,
} from './Ast';
import {
  ExpectedBindingIdentifier,
  ExpectedClassMember,
  ExpectedDeclaration,
  ExpectedEnd,
  ExpectedExpression,
  ExpectedGlyph,
  ExpectedIdentifier,
  ExpectedKeyword,
  ExpectedLineSeparator,
  ExpectedPattern,
  ExpectedType,
  ExpressionIntoPatternError,
  ParserError,
  UnexpectedTokenError,
} from './Error';
import {BindingIdentifier} from './Identifier';
import {Glyph, IdentifierToken, Lexer, Token, TokenType} from './Lexer';
import {Loc} from './Loc';

export function parseDeclaration(
  lexer: Lexer
): Result<Declaration, ParserError> {
  try {
    const parser = new Parser(lexer);
    const declaration = parser.parseDeclaration();
    parser.parseEnding();
    return Ok(declaration);
  } catch (error) {
    if (error instanceof Error) throw error;
    return Err(error);
  }
}

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

export function parseStatement(lexer: Lexer): Result<Statement, ParserError> {
  try {
    const parser = new Parser(lexer);
    const statement = parser.parseStatement();
    parser.parseEnding();
    return Ok(statement);
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
   * Parses a declaration.
   */
  parseDeclaration(): Declaration {
    let token1 = this.nextToken();
    let token2 = this.peekToken();

    // The default access level is private.
    let access = Access.Private;

    // Use a loop to parse common declaration modifiers like the access modifier
    // and decorator modifiers.
    while (true) {
      // Throw an error if our first token is not an identifier.
      if (token1.type !== TokenType.Identifier) {
        throw UnexpectedTokenError(token1, ExpectedDeclaration);
      }

      // If the token after our identifier suggests that we have a function
      // declaration then parse that `FunctionDeclaration`.
      if (
        token2.type === TokenType.Glyph &&
        (token2.glyph === Glyph.ParenLeft || token2.glyph === Glyph.LessThan)
      ) {
        const name = Name(token1.loc, token1.identifier);
        const f = this.parseFunction();
        return FunctionDeclaration(
          access,
          name,
          f.typeParameters,
          f.parameters,
          f.return,
          f.body
        );
      }

      // First, if we have an access modifier then parse it. After parsing our
      // modifier proceed to the next token.
      if (token1.identifier === 'public') {
        access = Access.Public;
        token1 = this.nextToken();
        token2 = this.peekToken();
        continue;
      } else if (token1.identifier === 'protected') {
        access = Access.Protected;
        token1 = this.nextToken();
        token2 = this.peekToken();
        continue;
      } else if (token1.identifier === 'private') {
        access = Access.Private;
        token1 = this.nextToken();
        token2 = this.peekToken();
        continue;
      }

      // Parse a `TypeDeclaration`.
      if (token1.identifier === 'type') {
        const name = this.parseName();
        return this.parseTypeDeclaration(access, name);
      }

      // Parse a `ClassDeclaration`.
      if (token1.identifier === 'class') {
        const name = this.parseName();
        return this.parseClassDeclaration(access, name, false, false);
      }

      // Parse a `BaseClassDeclaration`.
      if (token1.identifier === 'base') {
        this.parseKeyword('class');
        const name = this.parseName();
        return this.parseClassDeclaration(access, name, true, false);
      }

      // Parse an unsealed `BaseClassDeclaration`.
      if (token1.identifier === 'unsealed') {
        this.parseKeyword('base');
        this.parseKeyword('class');
        const name = this.parseName();
        return this.parseClassDeclaration(access, name, true, true);
      }

      // Parse an `InterfaceDeclaration`.
      if (token1.identifier === 'interface') {
        throw new Error('unimplemented');
      }

      // Parse a `NamespaceDeclaration`.
      if (token1.identifier === 'namespace') {
        throw new Error('unimplemented');
      }

      // Throw an error if we don’t recognize any of the identifiers that
      // we parsed.
      throw UnexpectedTokenError(token1, ExpectedDeclaration);
    }
  }

  /**
   * Finishes parsing a `TypeDeclaration`.
   */
  parseTypeDeclaration(access: Access, name: Name): TypeDeclaration {
    // Parse type parameters if some are available right after the name.
    let typeParameters: ReadonlyArray<TypeParameter> = [];
    if (this.tryParseGlyph(Glyph.LessThan)) {
      typeParameters = this.parseCommaList(
        () => this.parseGenericParameter(),
        Glyph.GreaterThan
      );
      this.nextToken();
    }

    // Parse an equals sign after the name and optional type parameters.
    this.parseGlyph(Glyph.Equals);

    // Parse the type part of the type declaration.
    const value = this.parseType();

    return TypeDeclaration(access, name, typeParameters, value);
  }

  /**
   * Finishes parsing a `ClassDeclaration`.
   */
  parseClassDeclaration(
    access: Access,
    name: Name,
    base: boolean,
    unsealed: boolean
  ): ClassDeclaration {
    // TODO:
    //
    // - Parse body.
    // - Parse extends.
    // - Parse implements.
    // - Parse constructor pattern.

    // Try to parse type parameters if available.
    let typeParameters: ReadonlyArray<TypeParameter> = [];
    if (this.tryParseGlyph(Glyph.LessThan)) {
      typeParameters = this.parseCommaList(
        () => this.parseGenericParameter(),
        Glyph.GreaterThan
      );
      this.nextToken();
    }

    // Try to parse function parameters if available.
    let parameters: ReadonlyArray<FunctionParameter> = [];
    if (this.tryParseGlyph(Glyph.ParenLeft)) {
      parameters = this.parseCommaList(
        () => this.parseFunctionParameter(),
        Glyph.ParenRight
      );
      this.nextToken();
    }

    // Try to parse a class body if available.
    let body: ReadonlyArray<ClassMember> = [];
    if (this.tryParseGlyph(Glyph.BraceLeft)) {
      body = this.parseLineSeparatorList(
        () => this.parseClassMember(),
        Glyph.BraceRight
      );
      this.nextToken();
    }

    // Return the final class.
    return ClassDeclaration({
      access,
      name,
      base,
      unsealed,
      typeParameters,
      parameters,
      extends: undefined,
      implements: [],
      body,
    });
  }

  /**
   * Parses the `ClassMember` grammar.
   */
  parseClassMember(): ClassMember {
    let token1 = this.nextToken();
    let token2 = this.peekToken();

    // The default access level is private.
    let access = Access.Private;

    // Whether or not this is a base member.
    let base = false;

    // Use a loop to parse common declaration modifiers like the access modifier
    // and decorator modifiers.
    while (true) {
      // Throw an error if our first token is not an identifier.
      if (token1.type !== TokenType.Identifier) {
        throw UnexpectedTokenError(token1, ExpectedClassMember);
      }

      // If the token after our identifier suggests that we have a function
      // declaration then parse that `FunctionDeclaration`.
      if (
        token2.type === TokenType.Glyph &&
        (token2.glyph === Glyph.ParenLeft || token2.glyph === Glyph.LessThan)
      ) {
        const name = Name(token1.loc, token1.identifier);
        const f = this.parseFunctionWithOptionalBody();
        return ClassMethod({
          access,
          base,
          name,
          typeParameters: f.typeParameters,
          parameters: f.parameters,
          return: f.return,
          body: f.body,
        });
      }

      // First, if we have an access modifier then parse it. After parsing our
      // modifier proceed to the next token. If we have already parsed the
      // `base` modifier then we may not parse an access modifier.
      if (!base) {
        if (token1.identifier === 'public') {
          access = Access.Public;
          token1 = this.nextToken();
          token2 = this.peekToken();
          continue;
        } else if (token1.identifier === 'protected') {
          access = Access.Protected;
          token1 = this.nextToken();
          token2 = this.peekToken();
          continue;
        } else if (token1.identifier === 'private') {
          access = Access.Private;
          token1 = this.nextToken();
          token2 = this.peekToken();
          continue;
        }
      }

      // If we have a base modifier then parse it. After parsing our modifier
      // proceed to the next token.
      if (token1.identifier === 'base') {
        base = true;
        token1 = this.nextToken();
        token2 = this.peekToken();
        continue;
      }

      // Throw an error if we don’t recognize anything that we parsed.
      throw UnexpectedTokenError(token1, ExpectedClassMember);
    }
  }

  /**
   * Parses the `Function` grammar.
   */
  parseFunction(): {
    readonly typeParameters: ReadonlyArray<TypeParameter>;
    readonly parameters: ReadonlyArray<FunctionParameter>;
    readonly return: Type | undefined;
    readonly body: Expression;
  } {
    // Try to parse type parameters if available.
    let typeParameters: ReadonlyArray<TypeParameter> = [];
    if (this.tryParseGlyph(Glyph.LessThan)) {
      typeParameters = this.parseCommaList(
        () => this.parseGenericParameter(),
        Glyph.GreaterThan
      );
      this.nextToken();
    }

    // Parse an opening parentheses.
    this.parseGlyph(Glyph.ParenLeft);

    // Parse the function declaration parameters. They are patterns with an
    // optional type annotation.
    const parameters = this.parseCommaList(
      () => this.parseFunctionParameter(),
      Glyph.ParenRight
    );
    this.nextToken();

    // Parse an optional type annotation for the function.
    const ret = this.tryParseGlyph(Glyph.Colon)
      ? this.parsePrimaryType()
      : undefined;

    // Parse the function declaration arrow.
    this.parseGlyph(Glyph.Arrow);

    // Parse the function’s expression body.
    const body = this.parseExpression();

    return {
      typeParameters,
      parameters,
      return: ret,
      body,
    };
  }

  /**
   * Parses the `Function` or `FunctionWithoutBody` grammar.
   */
  parseFunctionWithOptionalBody(): {
    readonly typeParameters: ReadonlyArray<TypeParameter>;
    readonly parameters: ReadonlyArray<FunctionParameter>;
    readonly return: Type | undefined;
    readonly body: Expression | undefined;
  } {
    // Try to parse type parameters if available.
    let typeParameters: ReadonlyArray<TypeParameter> = [];
    if (this.tryParseGlyph(Glyph.LessThan)) {
      typeParameters = this.parseCommaList(
        () => this.parseGenericParameter(),
        Glyph.GreaterThan
      );
      this.nextToken();
    }

    // Parse an opening parentheses.
    this.parseGlyph(Glyph.ParenLeft);

    // Parse the function declaration parameters. They are patterns with an
    // optional type annotation.
    const parameters = this.parseCommaList(
      () => this.parseFunctionParameter(),
      Glyph.ParenRight
    );
    this.nextToken();

    // Parse an optional type annotation for the function.
    const ret = this.tryParseGlyph(Glyph.Colon)
      ? this.parsePrimaryType()
      : undefined;

    // If we have a return type annotation then the body for this function
    // is optional. Otherwise it is required.
    let body: Expression | undefined;
    if (ret !== undefined) {
      if (this.tryParseGlyph(Glyph.Arrow)) {
        body = this.parseExpression();
      }
    } else {
      this.parseGlyph(Glyph.Arrow);
      body = this.parseExpression();
    }

    return {
      typeParameters,
      parameters,
      return: ret,
      body,
    };
  }

  /**
   * Parses the `FunctionParameter` grammar.
   */
  parseFunctionParameter(): FunctionParameter {
    const pattern = this.parsePattern();
    const type = this.tryParseGlyph(Glyph.Colon) ? this.parseType() : undefined;
    return FunctionParameter(pattern, type);
  }

  /**
   * Parses the `Type` grammar.
   */
  parseType(): Type {
    return this.parseFunctionTypeExtension(this.parsePrimaryType());
  }

  /**
   * Parse the `PrimaryType` grammar.
   */
  parsePrimaryType(): Type {
    const token = this.nextToken();

    // Assign primary types here and we will parse extensions on those types at
    // the end of this function. Return non-primary types.
    let primaryType: Type;

    if (
      // Parse `ReferenceType`.
      token.type === TokenType.Identifier
    ) {
      const identifier = BindingIdentifier.create(token.identifier);
      if (identifier !== undefined) {
        primaryType = ReferenceType(token.loc, identifier);
      } else {
        throw UnexpectedTokenError(token, ExpectedType);
      }
    } else if (
      // Parse `UnitType`, `TupleType`, and `WrappedType`.
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

      // Finish parsing either `UnitType`, `TupleType`, or `WrappedType`.
      const loc = new Loc(start, end);
      if (types.length === 0) {
        primaryType = UnitType(loc);
      } else if (types.length === 1) {
        primaryType = WrappedType(loc, types[0]);
      } else {
        primaryType = TupleType(loc, Array2.create(types));
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
   * Tries to parse a `FunctionType` by checking if there is an arrow and
   * converting the right types. The parameter should be a `PrimaryType`.
   */
  parseFunctionTypeExtension(type: Type): Type {
    // Try to parse a `FunctionType`.
    const nextToken = this.peekToken();
    if (nextToken.type === TokenType.Glyph && nextToken.glyph === Glyph.Arrow) {
      // Parse `FunctionType` shorthand.
      if (type.kind === TypeKind.Reference) {
        this.nextToken();
        const body = this.parseType();
        const loc = new Loc(type.loc.start, body.loc.end);
        return FunctionType(loc, [type], body);
      }

      // Parse `FunctionType` with no arguments.
      if (type.kind === TypeKind.Unit) {
        this.nextToken();
        const body = this.parseType();
        const loc = new Loc(type.loc.start, body.loc.end);
        return FunctionType(loc, [], body);
      }

      // Parse `FunctionType` with one argument.
      if (type.kind === TypeKind.Wrapped) {
        this.nextToken();
        const body = this.parseType();
        const loc = new Loc(type.loc.start, body.loc.end);
        return FunctionType(loc, [type.type], body);
      }

      // Parse `FunctionType` with many arguments.
      if (type.kind === TypeKind.Tuple) {
        this.nextToken();
        const body = this.parseType();
        const loc = new Loc(type.loc.start, body.loc.end);
        return FunctionType(loc, type.elements, body);
      }
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
    let typeParameters: Array<Type> | Array1<Type> = [];
    const token = this.peekToken();
    if (token.type === TokenType.Glyph && token.glyph === Glyph.Colon) {
      this.nextToken();
      typeParameters = this.parseNonEmptyList(
        () => this.parseType(),
        Glyph.Plus
      );
    }

    return TypeParameter(name, typeParameters);
  }

  /**
   * Parses the `Statement` grammar.
   */
  parseStatement(): Statement {
    // Try to parse the `Statement`s which don’t look anything
    // like `Expression`s.
    const statement = this.tryParseDistinctStatement();
    if (statement !== undefined) return statement;

    // All our remaining statements look like expressions.
    const expression = this.parseExpression();

    const token = this.peekToken();

    // If we see a colon then we expect that we are a `BindingStatement` with a
    // type annotation.
    if (token.type === TokenType.Glyph && token.glyph === Glyph.Colon) {
      this.nextToken();
      const type = this.parseType();

      // We expect an equality token next. If we do not see one then throw an
      // unexpected token error.
      const equalsToken = this.nextToken();
      if (
        equalsToken.type !== TokenType.Glyph ||
        equalsToken.glyph !== Glyph.Equals
      ) {
        throw UnexpectedTokenError(equalsToken, ExpectedGlyph(Glyph.Equals));
      }

      // Convert our expression into a pattern with the equals token as the
      // reason in case we can’t convert the expression.
      const binding = expressionIntoPattern(equalsToken, expression);

      // Parse the value of this binding statement.
      const value = this.parseExpression();

      return BindingStatement(binding, type, value);
    }

    // If we see an equals then we expect that we are a `BindingStatement`
    // without a type annotation.
    if (token.type === TokenType.Glyph && token.glyph === Glyph.Equals) {
      this.nextToken();

      // Convert our expression into a pattern with the equals token as the
      // reason in case we can’t convert the expression.
      const binding = expressionIntoPattern(token, expression);

      // Parse the value of this binding statement.
      const value = this.parseExpression();

      return BindingStatement(binding, undefined, value);
    }

    return ExpressionStatement(expression);
  }

  /**
   * Tries to parse `Statement` grammars which are distinct from the
   * `Expression` grammar.
   */
  tryParseDistinctStatement(): Statement | undefined {
    const token = this.peekToken();
    if (token.type === TokenType.Identifier) {
      if (
        // Parse a `WhileLoopStatement`.
        token.identifier === 'while'
      ) {
        this.nextToken();
        const test = this.parseExpression();
        this.parseKeyword('do');
        const body = this.parseExpression();
        return WhileLoopStatement(test, body);
      } else if (
        // Parse a `ForLoopStatement`.
        token.identifier === 'for'
      ) {
        this.nextToken();
        const binding = this.parsePattern();
        this.parseKeyword('in');
        const iterable = this.parseExpression();
        this.parseKeyword('do');
        const body = this.parseExpression();
        return ForLoopStatement(binding, iterable, body);
      } else {
        return undefined;
      }
    } else {
      return undefined;
    }
  }

  /**
   * Parses the `Expression` grammar. Extra care must be taken to properly parse
   * the `ExpressionWithTypeAnnotation` grammar.
   */
  parseExpression(
    /**
     * Option to help us parse the `ExpressionWithTypeAnnotation` grammar. If we
     * see a colon after `FunctionParameters` then we either have an annotation
     * or a `FunctionExpression` return type. If we were not provided
     * `setTypeAnnotation` then we assume we have a `FunctionExpression`. If we
     * were provided `setTypeAnnotation` and we don’t find an arrow after
     * parsing the type annotation type then we call the function with the type
     * we parsed so the caller may use the type annotation.
     */
    setTypeAnnotation?: ((type: Type) => void) | undefined
  ): Expression {
    const token = this.peekToken();

    // If we have debug assertions enabled then test `startsExpression()` by
    // preemptively throwing an `UnexpectedTokenError`.
    if (__DEBUG__) {
      if (!startsExpression(token)) {
        throw UnexpectedTokenError(token, ExpectedExpression);
      }
    }

    if (token.type === TokenType.Identifier) {
      // Parse `ConditionalExpression`.
      if (token.identifier === 'if') {
        const start = this.nextToken().loc.start;

        // Parse the test expression for the conditional and the consequent
        // expression. Then we need to check for an optional
        // alternate expression.
        const test = this.parseExpression();
        this.parseKeyword('then');

        // If we have `setTypeAnnotation` then a conditional expression is a bit
        // special since the last expression may either be the `then` or `else`.
        if (!setTypeAnnotation) {
          const consequent = this.parseExpression();

          // If we have an `else` keyword then we have an alternate expression.
          const alternate = this.tryParseKeyword('else')
            ? this.parseExpression()
            : undefined;

          const end = (alternate ? alternate : consequent).loc.end;
          const loc = new Loc(start, end);
          return ConditionalExpression(loc, test, consequent, alternate);
        } else {
          let type: Type | undefined;

          const consequent = this.parseExpression(t => (type = t));

          // If we have an `else` keyword and we did not parse a type with our
          // consequent then we have an alternate expression.
          const alternate =
            type === undefined && this.tryParseKeyword('else')
              ? this.parseExpression(t => (type = t))
              : undefined;

          // If we parsed a type annotation then call `setTypeAnnotation`
          // with it.
          if (type !== undefined) setTypeAnnotation(type);

          const end = (alternate ? alternate : consequent).loc.end;
          const loc = new Loc(start, end);
          return ConditionalExpression(loc, test, consequent, alternate);
        }
      }

      // Parse `ReturnExpression`.
      if (token.identifier === 'return') {
        this.nextToken();
        const nextToken = this.peekToken();
        if (
          token.loc.end.line === nextToken.loc.start.line &&
          startsExpression(nextToken)
        ) {
          const argument = this.parseExpression(setTypeAnnotation);
          const loc = new Loc(token.loc.start, argument.loc.end);
          return ReturnExpression(loc, argument);
        } else {
          return ReturnExpression(token.loc, undefined);
        }
      }

      // Parse `BreakExpression`.
      if (token.identifier === 'break') {
        this.nextToken();
        const nextToken = this.peekToken();
        if (
          token.loc.end.line === nextToken.loc.start.line &&
          startsExpression(nextToken)
        ) {
          const argument = this.parseExpression(setTypeAnnotation);
          const loc = new Loc(token.loc.start, argument.loc.end);
          return BreakExpression(loc, argument);
        } else {
          return BreakExpression(token.loc, undefined);
        }
      }

      // Parse `ContinueExpression`.
      if (token.identifier === 'continue') {
        return ContinueExpression(this.nextToken().loc);
      }

      // Parse `LoopExpression`.
      if (token.identifier === 'loop') {
        const start = this.nextToken().loc.start;
        const body = this.parseExpression(setTypeAnnotation);
        const loc = new Loc(start, body.loc.end);
        return LoopExpression(loc, body);
      }
    }

    // Parse `FunctionExpression` with generic parameters.
    if (token.type === TokenType.Glyph && token.glyph === Glyph.LessThan) {
      // Parse the generic type parameters for our function expression.
      this.nextToken();
      const typeParameters = this.parseCommaList(
        () => this.parseGenericParameter(),
        Glyph.GreaterThan
      );
      this.nextToken();

      // Parse the value parameters for our function expression.
      this.parseGlyph(Glyph.ParenLeft);
      const parameters = this.parseCommaList(
        () => this.parseFunctionParameter(),
        Glyph.ParenRight
      );
      this.nextToken();

      // Parse an optional type annotation for the function.
      const ret = this.tryParseGlyph(Glyph.Colon)
        ? this.parsePrimaryType()
        : undefined;

      // Parse the body of our function expression.
      this.parseGlyph(Glyph.Arrow);
      const body = this.parseExpression(setTypeAnnotation);

      const loc = new Loc(token.loc.start, body.loc.end);
      return FunctionExpression(loc, typeParameters, parameters, ret, body);
    }

    const expression = this.parseBinaryExpression();

    // Parse `FunctionExpression` shorthand.
    if (
      expression.kind === ExpressionKind.Reference &&
      this.tryParseGlyph(Glyph.Arrow)
    ) {
      const parameter = FunctionParameter(
        BindingPattern(expression.loc, expression.identifier)
      );
      const body = this.parseExpression(setTypeAnnotation);
      const loc = new Loc(expression.loc.start, body.loc.end);
      return FunctionExpression(loc, [], [parameter], undefined, body);
    }

    // Try to parse a `FunctionExpression` if we parsed an expression that looks
    // a little bit like function parameters.
    if (
      expression.kind === ExpressionKind.Unit ||
      expression.kind === ExpressionKind.Wrapped ||
      expression.kind === ExpressionKind.Tuple
    ) {
      // We may have a `FunctionExpression` with an annotated return type or an
      // annotated expression.
      //
      // Note that we parse a `PrimaryType`. This is because according to the
      // `ExpressionWithTypeAnnotation` grammar unit, wrapped, and tuple
      // expressions may only be annotated with `PrimaryType` to avoid ambiguity
      // with `FunctionExpression`.
      const type = this.tryParseGlyph(Glyph.Colon)
        ? this.parsePrimaryType()
        : undefined;

      // Look ahead at the next token and determine if it is an arrow.
      const nextToken = this.peekToken();

      // Deal with the fact that we don’t have an arrow as the next token. Gets
      // complicated if we also have a type annotation.
      if (
        !(nextToken.type === TokenType.Glyph && nextToken.glyph === Glyph.Arrow)
      ) {
        if (type) {
          if (setTypeAnnotation) {
            // If we have `setTypeAnnotation` then the arrow is optional. We may
            // treat the parsed type as an annotation.
            setTypeAnnotation(type);
            return expression;
          } else {
            // An arrow is required after a type annotation if we don’t
            // have `setTypeAnnotation`.
            throw UnexpectedTokenError(nextToken, ExpectedGlyph(Glyph.Arrow));
          }
        } else {
          // If we don’t have an arrow but we also don’t have a type annotation,
          // then return the expression as-is.
          return expression;
        }
      } else {
        this.nextToken();

        // Parse `FunctionExpression` with no arguments.
        if (expression.kind === ExpressionKind.Unit) {
          const body = this.parseExpression(setTypeAnnotation);
          const loc = new Loc(expression.loc.start, body.loc.end);
          return FunctionExpression(loc, [], [], type, body);
        }

        // Parse `FunctionExpression` with one argument.
        if (expression.kind === ExpressionKind.Wrapped) {
          const parameter = FunctionParameter(
            expressionIntoPattern(nextToken, expression.expression),
            expression.type
          );
          const body = this.parseExpression(setTypeAnnotation);
          const loc = new Loc(expression.loc.start, body.loc.end);
          return FunctionExpression(loc, [], [parameter], type, body);
        }

        // Parse `FunctionExpression` with many arguments.
        if (expression.kind === ExpressionKind.Tuple) {
          const parameters = expression.elements.map(element =>
            FunctionParameter(
              expressionIntoPattern(nextToken, element.expression),
              element.type
            )
          );
          const body = this.parseExpression(setTypeAnnotation);
          const loc = new Loc(expression.loc.start, body.loc.end);
          return FunctionExpression(loc, [], parameters, type, body);
        }

        // This is unreachable. Cast to `never` to assert this and return
        // `never` to make TypeScript happy.
        const unreachable: never = expression;
        return unreachable;
      }
    } else {
      return expression;
    }
  }

  /**
   * Parse the `BinaryExpression` grammar.
   */
  parseBinaryExpression(): Expression {
    return this.parseBinaryExpressionOperator(0, this.parseUnaryExpression());
  }

  /**
   * Parse a binary expression operator with our precedence algorithm. The
   * `precedence` parameter provides context into which operators we are allowed
   * to parse.
   */
  parseBinaryExpressionOperator(
    precedence: number,
    left: Expression,
    noGreaterThan: boolean = false
  ): Expression {
    // Parse a `LogicalExpressionOr`.
    if (precedence <= LogicalOrPrecedence) {
      if (this.tryParseGlyph(Glyph.BarDouble)) {
        const op = LogicalExpressionOperator.Or;
        const right = this.parseBinaryExpressionOperator(
          LogicalOrPrecedence + 1,
          this.parseUnaryExpression()
        );
        const loc = new Loc(left.loc.start, right.loc.end);
        const node = LogicalExpression(loc, op, left, right);
        return this.parseBinaryExpressionOperator(precedence, node);
      }
    }

    // Parse a `LogicalExpressionAnd`.
    if (precedence <= LogicalAndPrecedence) {
      if (this.tryParseGlyph(Glyph.AmpersandDouble)) {
        const op = LogicalExpressionOperator.And;
        const right = this.parseBinaryExpressionOperator(
          LogicalAndPrecedence + 1,
          this.parseUnaryExpression()
        );
        const loc = new Loc(left.loc.start, right.loc.end);
        const node = LogicalExpression(loc, op, left, right);
        return this.parseBinaryExpressionOperator(precedence, node);
      }
    }

    // Parse an `EqualityExpression`.
    if (precedence <= EqualityPrecedence) {
      let op: BinaryExpressionOperator | undefined;
      if (this.tryParseGlyph(Glyph.EqualsDouble)) {
        op = BinaryExpressionOperator.Equal;
      } else if (this.tryParseGlyph(Glyph.NotEquals)) {
        op = BinaryExpressionOperator.EqualNot;
      }
      if (op !== undefined) {
        const right = this.parseBinaryExpressionOperator(
          EqualityPrecedence + 1,
          this.parseUnaryExpression()
        );
        const loc = new Loc(left.loc.start, right.loc.end);
        const node = BinaryExpression(loc, op, left, right);
        return this.parseBinaryExpressionOperator(precedence, node);
      }
    }

    // Parse a `RelationalExpression`.
    if (precedence <= RelationalPrecedence) {
      let op: BinaryExpressionOperator | undefined;
      if (this.tryParseGlyph(Glyph.LessThan)) {
        op = BinaryExpressionOperator.LessThan;
      } else if (!noGreaterThan && this.tryParseGlyph(Glyph.GreaterThan)) {
        op = BinaryExpressionOperator.GreaterThan;
      } else if (this.tryParseGlyph(Glyph.LessThanOrEqual)) {
        op = BinaryExpressionOperator.LessThanOrEqual;
      } else if (this.tryParseGlyph(Glyph.GreaterThanOrEqual)) {
        op = BinaryExpressionOperator.GreaterThanOrEqual;
      }
      if (op !== undefined) {
        const right = this.parseBinaryExpressionOperator(
          RelationalPrecedence + 1,
          this.parseUnaryExpression()
        );
        const loc = new Loc(left.loc.start, right.loc.end);
        const node = BinaryExpression(loc, op, left, right);
        return this.parseBinaryExpressionOperator(
          precedence,
          node,
          op === BinaryExpressionOperator.LessThan
        );
      }
    }

    // Parse `PatternExpression`.
    if (precedence <= PatternPrecedence) {
      if (this.tryParseKeywordOnSameLine('is')) {
        const pattern = this.parsePattern();
        const loc = new Loc(left.loc.start, pattern.loc.end);
        const node = PatternExpression(loc, left, pattern);
        return this.parseBinaryExpressionOperator(PatternPrecedence + 1, node);
      }
    }

    // Parse an `AdditiveExpression`.
    if (precedence <= AdditivePrecedence) {
      let op: BinaryExpressionOperator | undefined;
      if (this.tryParseGlyph(Glyph.Plus)) {
        op = BinaryExpressionOperator.Add;
      } else if (this.tryParseGlyph(Glyph.Minus)) {
        op = BinaryExpressionOperator.Subtract;
      }
      if (op !== undefined) {
        const right = this.parseBinaryExpressionOperator(
          AdditivePrecedence + 1,
          this.parseUnaryExpression()
        );
        const loc = new Loc(left.loc.start, right.loc.end);
        const node = BinaryExpression(loc, op, left, right);
        return this.parseBinaryExpressionOperator(precedence, node);
      }
    }

    // Parse a `MultiplicativeExpression`.
    if (precedence <= MultiplicativePrecedence) {
      let op: BinaryExpressionOperator | undefined;
      if (this.tryParseGlyph(Glyph.Percent)) {
        op = BinaryExpressionOperator.Remainder;
      } else if (this.tryParseGlyph(Glyph.Asterisk)) {
        op = BinaryExpressionOperator.Multiply;
      } else if (this.tryParseGlyph(Glyph.Slash)) {
        op = BinaryExpressionOperator.Divide;
      }
      if (op !== undefined) {
        const right = this.parseBinaryExpressionOperator(
          MultiplicativePrecedence + 1,
          this.parseUnaryExpression()
        );
        const loc = new Loc(left.loc.start, right.loc.end);
        const node = BinaryExpression(loc, op, left, right);
        return this.parseBinaryExpressionOperator(precedence, node);
      }
    }

    return left;
  }

  /**
   * Parse the `UnaryExpression` grammar.
   */
  parseUnaryExpression(): Expression {
    const token = this.nextToken();

    // Parse negative `UnaryExpression`.
    if (token.type === TokenType.Glyph && token.glyph === Glyph.Minus) {
      const argument = this.parseUnaryExpression();
      const loc = new Loc(token.loc.start, argument.loc.end);
      return UnaryExpression(loc, UnaryExpressionOperator.Negative, argument);
    }

    // Parse not `UnaryExpression`.
    if (token.type === TokenType.Glyph && token.glyph === Glyph.Exclamation) {
      const argument = this.parseUnaryExpression();
      const loc = new Loc(token.loc.start, argument.loc.end);
      return UnaryExpression(loc, UnaryExpressionOperator.Not, argument);
    }

    let primaryExpression: Expression;

    // Parse `ReferenceExpression` and `BindingHolePattern`.
    if (token.type === TokenType.Identifier) {
      const identifier = BindingIdentifier.create(token.identifier);
      if (identifier !== undefined) {
        primaryExpression = ReferenceExpression(token.loc, identifier);
      } else if (token.identifier === '_') {
        primaryExpression = HoleExpression(token.loc);
      } else if (
        // Parse `MatchExpression`.
        token.identifier === 'case'
      ) {
        const start = token.loc.start;

        // First, we need to parse the expression that we are testing in this
        // match. After that we expect a colon to separate the match body.
        const test = this.parseExpression();
        this.parseKeyword('of');
        this.parseGlyph(Glyph.ParenLeft);

        // Parse all that match cases inside this match expression.
        const cases = this.parseLineSeparatorList(() => {
          // Parse a non-empty list of bindings separated by a single bar (`|`).
          // This way multiple patterns can match to a single body.
          const bindings = this.parseNonEmptyList(
            () => this.parsePattern(),
            Glyph.Bar
          );

          // If we see the binding keyword `if` then we have some condition on
          // this match case.
          let test: Expression | undefined;
          if (this.tryParseKeyword('if')) {
            test = this.parseBinaryExpression();
          }

          // Parse the arrow and then the expression body of this case which
          // will be executed if any of the patterns match.
          this.parseGlyph(Glyph.Arrow);
          const body = this.parseExpression();

          return MatchCase(bindings, test, body);
        }, Glyph.ParenRight);

        // Finally, create the match expression.
        const end = this.nextToken().loc.end;
        const loc = new Loc(start, end);
        primaryExpression = MatchExpression(loc, test, cases);
      } else {
        throw UnexpectedTokenError(token, ExpectedExpression);
      }
    } else if (
      // Parse `BlockExpression`, `UnitExpression`, `TupleExpression`,
      // and `WrappedExpression`.
      token.type === TokenType.Glyph &&
      token.glyph === Glyph.ParenLeft
    ) {
      const start = token.loc.start;

      // There are many possibilities for an expression that starts with an
      // opening parentheses. We might have:
      //
      // 1. A `BlockExpression` if inside there is one or more statements.
      //    However, a `BlockExpression` may not have an annotation and may not
      //    become a tuple.
      // 2. A `UnitExpression` if there are no tuples or statements.
      // 3. A `TupleExpression` if there are multiple expressions with optional
      //    type annotations but no statements.
      // 4. A `WrappedExpression` if there is exactly one expression and an
      //    optional type annotation.
      //
      // So our parsing code is a bit tricky to make sure we cover all
      // these cases.

      // If we immediately close the parentheses then we have a unit expression.
      const nextToken = this.peekToken();
      if (
        nextToken.type === TokenType.Glyph &&
        nextToken.glyph === Glyph.ParenRight
      ) {
        const end = this.nextToken().loc.end;
        const loc = new Loc(start, end);
        primaryExpression = UnitExpression(loc);
      } else {
        // Otherwise we expect an expression maybe with an annotation.
        let firstType: Type | undefined;
        let firstStatement: Statement =
          this.tryParseDistinctStatement() ||
          ExpressionStatement(this.parseExpression(type => (firstType = type)));

        // If we did not already parse a type annotation, we see a colon, and we
        // have a colon then parse a type annotation.
        if (
          firstType === undefined &&
          firstStatement.kind === StatementKind.Expression &&
          this.tryParseGlyph(Glyph.Colon)
        ) {
          firstType = this.parseType();
        }

        // If the next token is an equals and our first statement was an
        // expression then we have a binding statement instead of a expression.
        let nextToken = this.peekToken();
        if (
          firstStatement.kind === StatementKind.Expression &&
          nextToken.type === TokenType.Glyph &&
          nextToken.glyph === Glyph.Equals
        ) {
          this.nextToken();
          const firstExpression = firstStatement.expression;
          const binding = expressionIntoPattern(nextToken, firstExpression);
          const value = this.parseExpression();
          firstStatement = BindingStatement(binding, firstType, value);
          firstType = undefined;
        }

        // Ok, so at this point there are a couple paths we could take:
        //
        // - A closing parentheses means we have a `WrappedExpression`.
        // - A comma means we have a `TupleExpression` or a `WrappedExpression`
        //   with a trailing comma.
        // - A semicolon means we have a `BlockExpression`.
        // - An unrecognized token on the next line means we have
        //   a `BlockExpression`.
        // - An equals token means we have a `BlockExpression` with
        //   a `BindingStatement`.
        nextToken = this.peekToken();
        if (
          // A closing parentheses means we have a `WrappedExpression`.
          firstStatement.kind === StatementKind.Expression &&
          nextToken.type === TokenType.Glyph &&
          nextToken.glyph === Glyph.ParenRight
        ) {
          const end = this.nextToken().loc.end;
          const loc = new Loc(start, end);
          primaryExpression = WrappedExpression(
            loc,
            firstStatement.expression,
            firstType
          );
        } else if (
          // A comma means we have a `TupleExpression` or a `WrappedExpression`
          // with a trailing comma.
          firstStatement.kind === StatementKind.Expression &&
          nextToken.type === TokenType.Glyph &&
          nextToken.glyph === Glyph.Comma
        ) {
          this.nextToken();
          const elements = [
            TupleExpressionElement(firstStatement.expression, firstType),
          ];
          this.parseCommaList(
            () => {
              let type: Type | undefined;
              const expression = this.parseExpression(t => (type = t));
              if (type === undefined && this.tryParseGlyph(Glyph.Colon)) {
                type = this.parseType();
              }
              return TupleExpressionElement(expression, type);
            },
            Glyph.ParenRight,
            elements
          );
          const end = this.nextToken().loc.end;
          const loc = new Loc(start, end);
          if (elements.length === 1) {
            primaryExpression = WrappedExpression(
              loc,
              firstStatement.expression,
              firstType
            );
          } else {
            primaryExpression = TupleExpression(loc, Array2.create(elements));
          }
        } else if (
          // A semicolon means we have a `BlockExpression`. However, if we
          // parsed a type annotation then we do not have a `BlockExpression` so
          // we should error instead.
          firstType === undefined &&
          nextToken.type === TokenType.Glyph &&
          nextToken.glyph === Glyph.Semicolon
        ) {
          this.nextToken();
          const statements = [firstStatement];
          this.parseLineSeparatorList(
            () => this.parseStatement(),
            Glyph.ParenRight,
            statements
          );
          const end = this.nextToken().loc.end;
          const loc = new Loc(start, end);
          primaryExpression = BlockExpression(loc, Array1.create(statements));
        } else if (
          // An unrecognized token on the next line means we have a
          // `BlockExpression`. However, if we parsed a type annotation then we
          // do not have a `BlockExpression` so we should error instead.
          firstType === undefined &&
          this.currentLoc.end.line !== nextToken.loc.start.line
        ) {
          const statements = [firstStatement];
          this.parseLineSeparatorList(
            () => this.parseStatement(),
            Glyph.ParenRight,
            statements
          );
          const end = this.nextToken().loc.end;
          const loc = new Loc(start, end);
          primaryExpression = BlockExpression(loc, Array1.create(statements));
        } else if (
          // If the next token is a closing parentheses and we do not have a
          // type annotation then we have a `BlockStatement`.
          firstType === undefined &&
          nextToken.type === TokenType.Glyph &&
          nextToken.glyph === Glyph.ParenRight
        ) {
          const end = this.nextToken().loc.end;
          const loc = new Loc(start, end);
          primaryExpression = BlockExpression(loc, [firstStatement]);
        } else {
          // If none of the above cases are true then throw an unexpected
          // token error.
          if (firstStatement.kind === StatementKind.Expression) {
            throw UnexpectedTokenError(nextToken, ExpectedGlyph(Glyph.Comma));
          } else {
            throw UnexpectedTokenError(nextToken, ExpectedLineSeparator);
          }
        }
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
      // Parse `MemberExpression`.
      if (this.tryParseGlyph(Glyph.Dot)) {
        const name = this.parseName();
        const loc = new Loc(primaryExpression.loc.start, name.loc.end);
        primaryExpression = MemberExpression(loc, primaryExpression, name);
        continue;
      }

      // Parse `CallExpression`.
      if (this.tryParseGlyphOnSameLine(Glyph.ParenLeft)) {
        const args = this.parseCommaList(
          () => this.parseExpression(),
          Glyph.ParenRight
        );
        const end = this.nextToken().loc.end;
        const loc = new Loc(primaryExpression.loc.start, end);
        primaryExpression = CallExpression(loc, primaryExpression, args);
        continue;
      }

      // NOTE: Currently `CallExpression` with `GenericArguments` is not
      // implemented. It would introduce a complicated cover grammar with
      // `RelationalExpression`.

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
        // Parse `BindingPattern` hole.
        if (token.identifier === '_') {
          return HolePattern(token.loc);
        } else {
          throw UnexpectedTokenError(token, ExpectedPattern);
        }
      }

      // If there is an `is` keyword on the same line as our binding identifier
      // then we have an `AliasPattern`.
      if (this.tryParseKeywordOnSameLine('is')) {
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
      if (this.tryParseGlyphOnSameLine(Glyph.ParenLeft)) {
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
   *
   * The optional third parameter allows you to supply an array which already
   * has some items.
   */
  parseCommaList<T>(
    parseItem: () => T,
    endGlyph: Glyph,
    items: Array<T> = []
  ): Array<T> {
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
   *
   * The optional third parameter allows you to supply an array which already
   * has some items.
   */
  parseLineSeparatorList<T>(
    parseItem: () => T,
    endGlyph: Glyph,
    items: Array<T> = []
  ): Array<T> {
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
   * Parses a non-empty list separated by the provided separator glyph. Does not
   * support trailing separators or empty lists.
   */
  parseNonEmptyList<T>(parseItem: () => T, separator: Glyph): Array1<T> {
    const firstItem = parseItem();
    const items: Array1<T> = [firstItem];

    while (this.tryParseGlyph(separator)) {
      const item = parseItem();
      items.push(item);
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
  tryParseGlyphOnSameLine(glyph: Glyph): boolean {
    const token = this.peekToken();
    if (
      token.type === TokenType.Glyph &&
      token.glyph === glyph &&
      token.loc.start.line === this.currentLoc.end.line
    ) {
      this.nextToken();
      return true;
    }
    return false;
  }

  /**
   * Parses a keyword. Throws an error if we could not parse the keyword.
   *
   * Note that the identifier does not necessarily need to be a keyword reserved
   * in `BindingIdentifier`.
   */
  parseKeyword(identifier: string) {
    const token = this.peekToken();
    if (
      token.type === TokenType.Identifier &&
      token.identifier === identifier
    ) {
      this.nextToken();
      return;
    }
    throw UnexpectedTokenError(token, ExpectedKeyword(identifier));
  }

  /**
   * Tries to parse a keyword. Returns true if we could parse it. Returns false
   * if we could not.
   *
   * Note that the identifier does not necessarily need to be a keyword reserved
   * in `BindingIdentifier`.
   */
  tryParseKeyword(identifier: string): boolean {
    const token = this.peekToken();
    if (
      token.type === TokenType.Identifier &&
      token.identifier === identifier
    ) {
      this.nextToken();
      return true;
    }
    return false;
  }

  /**
   * Tries to parse a keyword on the same line as the current position. Returns
   * true if we could parse it. Returns false if we could not.
   *
   * Note that the identifier does not necessarily need to be a keyword reserved
   * in `BindingIdentifier`.
   */
  tryParseKeywordOnSameLine(identifier: string): boolean {
    const token = this.peekToken();
    if (
      token.type === TokenType.Identifier &&
      token.identifier === identifier &&
      token.loc.start.line === this.currentLoc.end.line
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

/**
 * Does this token start an expression? We need this for correctly parsing
 * `return` and `break` expressions.
 *
 * It is dangerous for this function to match more then what characters exactly
 * start an expression! For example, say we included all identifiers in
 * `startsExpression()`. Then consider:
 *
 * ```ite
 * if x then return else y
 * ```
 *
 * Here the `return` would see that the next token is `else` which is an
 * identifier. It would try to parse that identifier as an expression when it is
 * not one!
 *
 * So be careful when adding to this function.
 */
function startsExpression(token: Token): boolean {
  return (
    (token.type === TokenType.Glyph &&
      (token.glyph === Glyph.ParenLeft ||
        token.glyph === Glyph.BraceLeft ||
        token.glyph === Glyph.BracketLeft ||
        token.glyph === Glyph.Minus ||
        token.glyph === Glyph.Exclamation ||
        token.glyph === Glyph.LessThan)) ||
    (token.type === TokenType.Identifier &&
      (!BindingIdentifier.isKeyword(token.identifier) ||
        token.identifier === '_' ||
        token.identifier === 'case' ||
        token.identifier === 'if' ||
        token.identifier === 'return' ||
        token.identifier === 'break' ||
        token.identifier === 'continue' ||
        token.identifier === 'loop'))
  );
}

// Binary operator precedence levels.
const LogicalOrPrecedence = 0;
const LogicalAndPrecedence = 1;
const EqualityPrecedence = 2;
const RelationalPrecedence = 3;
const PatternPrecedence = 4;
const AdditivePrecedence = 5;
const MultiplicativePrecedence = 6;

/**
 * Attempts to convert an expression into a pattern. Pattern syntax is a subset
 * of expression syntax. This allows us to write an efficient parser while also
 * supporting code like:
 *
 * ```ite
 * (a, b, c) // Expression
 * (a, b, c) = x // Pattern
 * (a, b, c) -> x // Pattern
 * ```
 *
 * This function throws an error if the expression is not a valid pattern. The
 * error includes the provided reason token.
 */
export function expressionIntoPattern(reason: Token, e: Expression): Pattern {
  switch (e.kind) {
    case ExpressionKind.Reference:
      return BindingPattern(e.loc, e.identifier);
    case ExpressionKind.Hole:
      return HolePattern(e.loc);
    case ExpressionKind.Unit:
      return UnitPattern(e.loc);
    case ExpressionKind.Tuple: {
      const elements = Array<TuplePatternElement>(e.elements.length);
      for (let i = 0; i < e.elements.length; i++) {
        const {expression, type} = e.elements[i];
        const pattern = expressionIntoPattern(reason, expression);
        elements[i] = TuplePatternElement(pattern, type);
      }
      return TuplePattern(e.loc, Array2.create(elements));
    }
    case ExpressionKind.Record: {
      if (e.extension !== undefined) {
        throw ExpressionIntoPatternError(e.extension, reason);
      }
      const properties = Array<RecordPatternProperty>(e.properties.length);
      for (let i = 0; i < e.properties.length; i++) {
        const {key, value, type, optional} = e.properties[i];
        const pattern = expressionIntoPattern(reason, value);
        properties[i] = RecordPatternProperty(key, pattern, type, {optional});
      }
      return RecordPattern(e.loc, properties);
    }
    case ExpressionKind.List: {
      const items = Array<Pattern>(e.items.length);
      for (let i = 0; i < e.items.length; i++) {
        const item = expressionIntoPattern(reason, e.items[i]);
        items[i] = item;
      }
      return ListPattern(e.loc, items);
    }
    case ExpressionKind.Member: {
      const identifiers: Array<Name> = [];
      const stack: Array<[boolean, MemberExpression]> = [[false, e]];
      while (stack.length !== 0) {
        const [done, member] = stack.pop()!; // tslint:disable-line no-non-null-assertion
        if (done === true) {
          identifiers.push(member.member);
        } else {
          stack.push([true, member]);
          const namespace = member.namespace;
          if (namespace.kind === ExpressionKind.Reference) {
            identifiers.push(Name(namespace.loc, namespace.identifier));
          } else if (namespace.kind === ExpressionKind.Member) {
            stack.push([false, namespace]);
          } else {
            throw ExpressionIntoPatternError(namespace, reason);
          }
        }
      }
      return QualifiedPattern(e.loc, Array2.create(identifiers));
    }
    case ExpressionKind.Call: {
      const identifiers: Array<Name> = [];
      const callee = e.callee;
      if (callee.kind === ExpressionKind.Reference) {
        identifiers.push(Name(callee.loc, callee.identifier));
      } else if (callee.kind === ExpressionKind.Member) {
        const stack: Array<[boolean, MemberExpression]> = [[false, callee]];
        while (stack.length !== 0) {
          const [done, member] = stack.pop()!; // tslint:disable-line no-non-null-assertion
          if (done === true) {
            identifiers.push(member.member);
          } else {
            stack.push([true, member]);
            const namespace = member.namespace;
            if (namespace.kind === ExpressionKind.Reference) {
              identifiers.push(Name(namespace.loc, namespace.identifier));
            } else if (namespace.kind === ExpressionKind.Member) {
              stack.push([false, namespace]);
            } else {
              throw ExpressionIntoPatternError(namespace, reason);
            }
          }
        }
      } else {
        throw ExpressionIntoPatternError(callee, reason);
      }
      const args = Array<Pattern>(e.arguments.length);
      for (let i = 0; i < e.arguments.length; i++) {
        const arg = expressionIntoPattern(reason, e.arguments[i]);
        args[i] = arg;
      }
      return DeconstructPattern(e.loc, Array1.create(identifiers), args);
    }
    case ExpressionKind.Pattern: {
      if (e.left.kind !== ExpressionKind.Reference) {
        throw ExpressionIntoPatternError(e.left, reason);
      }
      const alias = BindingName(e.left.loc, e.left.identifier);
      return AliasPattern(e.loc, alias, e.right);
    }
    case ExpressionKind.Wrapped: {
      const pattern = expressionIntoPattern(reason, e.expression);
      return WrappedPattern(e.loc, pattern, e.type);
    }
    case ExpressionKind.Function:
    case ExpressionKind.Conditional:
    case ExpressionKind.Match:
    case ExpressionKind.Return:
    case ExpressionKind.Break:
    case ExpressionKind.Continue:
    case ExpressionKind.Loop:
    case ExpressionKind.Logical:
    case ExpressionKind.Binary:
    case ExpressionKind.Unary:
    case ExpressionKind.Block:
      throw ExpressionIntoPatternError(e, reason);
  }
}
