import {Err, Ok, Result} from '../../Utils/Result';
import {
  BindingPattern,
  ForLoopStatement,
  ReferenceExpression,
  ReferenceType,
  Statement,
  WhileLoopStatement,
  WrappedPattern,
} from '../Ast';
import {
  ExpectedIdentifier,
  ExpectedKeyword,
  ParserError,
  UnexpectedTokenError,
} from '../Error';
import {Identifier, ident} from '../Identifier';
import {Glyph, GlyphToken, Lexer} from '../Lexer';
import {loc} from '../Loc';
import {parseStatement} from '../Parser';

function lex(source: string): Lexer {
  return Lexer.create(source);
}

describe('statement', () => {
  const cases: ReadonlyArray<{
    readonly source: string;
    readonly result: Result<Statement, ParserError>;
  }> = [
    {
      source: 'while x: y',
      result: Ok(
        WhileLoopStatement(
          ReferenceExpression(loc('7'), ident('x')),
          ReferenceExpression(loc('10'), ident('y'))
        )
      ),
    },
    {
      source: 'for x in y: z',
      result: Ok(
        ForLoopStatement(
          BindingPattern(loc('5'), ident('x')),
          ReferenceExpression(loc('10'), ident('y')),
          ReferenceExpression(loc('13'), ident('z'))
        )
      ),
    },
    {
      source: 'for x: T in y: z',
      result: Err(
        UnexpectedTokenError(
          GlyphToken(loc('6'), Glyph.Colon),
          ExpectedKeyword('in' as Identifier)
        )
      ),
    },
    {
      source: 'for (x: T) in y: z',
      result: Ok(
        ForLoopStatement(
          WrappedPattern(
            loc('5-10'),
            BindingPattern(loc('6'), ident('x')),
            ReferenceType(loc('9'), ident('T'))
          ),
          ReferenceExpression(loc('15'), ident('y')),
          ReferenceExpression(loc('18'), ident('z'))
        )
      ),
    },
  ];

  cases.forEach(({source, result}) => {
    test(source.replace(/\n/g, '\\n'), () => {
      expect(parseStatement(lex(source))).toEqual(result);
    });
  });
});
