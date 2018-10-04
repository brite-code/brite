import {Err, Ok, Result} from '../../Utils/Result';
import {
  BinaryExpression,
  BinaryExpressionOperator,
  BindingPattern,
  BindingStatement,
  ForLoopStatement,
  ReferenceExpression,
  ReferenceType,
  Statement,
  WhileLoopStatement,
  WrappedPattern,
} from '../Ast';
import {
  ExpectedGlyph,
  ExpectedKeyword,
  ExpressionIntoPatternError,
  ParserError,
  UnexpectedTokenError,
} from '../Error';
import {Identifier, ident} from '../Identifier';
import {EndToken, Glyph, GlyphToken, Lexer} from '../Lexer';
import {loc} from '../Loc';
import {parseStatement} from '../Parser';

const cases: ReadonlyArray<{
  readonly source: string;
  readonly result: Result<Statement, ParserError>;
}> = [
  {
    source: 'while x do y',
    result: Ok(
      WhileLoopStatement(
        ReferenceExpression(loc('7'), ident('x')),
        ReferenceExpression(loc('12'), ident('y'))
      )
    ),
  },
  {
    source: 'for x in y do z',
    result: Ok(
      ForLoopStatement(
        BindingPattern(loc('5'), ident('x')),
        ReferenceExpression(loc('10'), ident('y')),
        ReferenceExpression(loc('15'), ident('z'))
      )
    ),
  },
  {
    source: 'for x: T in y do z',
    result: Err(
      UnexpectedTokenError(
        GlyphToken(loc('6'), Glyph.Colon),
        ExpectedKeyword('in' as Identifier)
      )
    ),
  },
  {
    source: 'for (x: T) in y do z',
    result: Ok(
      ForLoopStatement(
        WrappedPattern(
          loc('5-10'),
          BindingPattern(loc('6'), ident('x')),
          ReferenceType(loc('9'), ident('T'))
        ),
        ReferenceExpression(loc('15'), ident('y')),
        ReferenceExpression(loc('20'), ident('z'))
      )
    ),
  },
  {
    source: 'x = y',
    result: Ok(
      BindingStatement(
        BindingPattern(loc('1'), ident('x')),
        undefined,
        ReferenceExpression(loc('5'), ident('y'))
      )
    ),
  },
  {
    source: 'x: T = y',
    result: Ok(
      BindingStatement(
        BindingPattern(loc('1'), ident('x')),
        ReferenceType(loc('4'), ident('T')),
        ReferenceExpression(loc('8'), ident('y'))
      )
    ),
  },
  {
    source: 'a + b = c',
    result: Err(
      ExpressionIntoPatternError(
        BinaryExpression(
          loc('1-5'),
          BinaryExpressionOperator.Add,
          ReferenceExpression(loc('1'), ident('a')),
          ReferenceExpression(loc('5'), ident('b'))
        ),
        GlyphToken(loc('7'), Glyph.Equals)
      )
    ),
  },
  {
    source: 'a + b: T = c',
    result: Err(
      ExpressionIntoPatternError(
        BinaryExpression(
          loc('1-5'),
          BinaryExpressionOperator.Add,
          ReferenceExpression(loc('1'), ident('a')),
          ReferenceExpression(loc('5'), ident('b'))
        ),
        GlyphToken(loc('10'), Glyph.Equals)
      )
    ),
  },
  {
    source: 'x: T',
    result: Err(
      UnexpectedTokenError(EndToken(loc('5')), ExpectedGlyph(Glyph.Equals))
    ),
  },
];

cases.forEach(({source, result}) => {
  test(source.replace(/\n/g, '\\n'), () => {
    expect(parseStatement(Lexer.create(source))).toEqual(result);
  });
});
