import {Err, Ok, Result} from '../../Utils/Result';
import {
  Access,
  BindingPattern,
  Declaration,
  FunctionDeclaration,
  FunctionExpression,
  FunctionParameter,
  FunctionType,
  Name,
  ReferenceExpression,
  ReferenceType,
  TypeParameter,
  WrappedExpression,
  WrappedType,
} from '../Ast';
import {
  ExpectedDeclaration,
  ExpectedGlyph,
  ParserError,
  UnexpectedTokenError,
} from '../Error';
import {Identifier, ident} from '../Identifier';
import {Glyph, GlyphToken, IdentifierToken, Lexer} from '../Lexer';
import {loc} from '../Loc';
import {parseDeclaration} from '../Parser';

const cases: ReadonlyArray<{
  readonly source: string;
  readonly result: Result<Declaration, ParserError>;
}> = [
  {
    source: 'f() -> x',
    result: Ok(
      FunctionDeclaration(
        Access.Private,
        Name(loc('1'), ident('f')),
        [],
        [],
        undefined,
        ReferenceExpression(loc('8'), ident('x'))
      )
    ),
  },
  {
    source: 'if() -> x',
    result: Ok(
      FunctionDeclaration(
        Access.Private,
        Name(loc('1-2'), 'if' as Identifier),
        [],
        [],
        undefined,
        ReferenceExpression(loc('9'), ident('x'))
      )
    ),
  },
  {
    source: 'class() -> x',
    result: Ok(
      FunctionDeclaration(
        Access.Private,
        Name(loc('1-5'), ident('class')),
        [],
        [],
        undefined,
        ReferenceExpression(loc('12'), ident('x'))
      )
    ),
  },
  {
    source: 'public() -> x',
    result: Ok(
      FunctionDeclaration(
        Access.Private,
        Name(loc('1-6'), ident('public')),
        [],
        [],
        undefined,
        ReferenceExpression(loc('13'), ident('x'))
      )
    ),
  },
  {
    source: 'public public() -> x',
    result: Ok(
      FunctionDeclaration(
        Access.Public,
        Name(loc('8-13'), ident('public')),
        [],
        [],
        undefined,
        ReferenceExpression(loc('20'), ident('x'))
      )
    ),
  },
  {
    source: 'data Foo',
    result: Err(
      UnexpectedTokenError(
        IdentifierToken(loc('6-8'), ident('Foo')),
        ExpectedDeclaration
      )
    ),
  },
  {
    source: '%',
    result: Err(
      UnexpectedTokenError(
        GlyphToken(loc('1'), Glyph.Percent),
        ExpectedDeclaration
      )
    ),
  },
  {
    source: 'public %',
    result: Err(
      UnexpectedTokenError(
        GlyphToken(loc('8'), Glyph.Percent),
        ExpectedDeclaration
      )
    ),
  },
  {
    source: 'f{',
    result: Err(
      UnexpectedTokenError(
        GlyphToken(loc('2'), Glyph.BraceLeft),
        ExpectedDeclaration
      )
    ),
  },
  {
    source: 'f<>{',
    result: Err(
      UnexpectedTokenError(
        GlyphToken(loc('4'), Glyph.BraceLeft),
        ExpectedGlyph(Glyph.ParenLeft)
      )
    ),
  },
  {
    source: 'f<>() -> x',
    result: Ok(
      FunctionDeclaration(
        Access.Private,
        Name(loc('1'), ident('f')),
        [],
        [],
        undefined,
        ReferenceExpression(loc('10'), ident('x'))
      )
    ),
  },
  {
    source: 'f<T>() -> x',
    result: Ok(
      FunctionDeclaration(
        Access.Private,
        Name(loc('1'), ident('f')),
        [TypeParameter(Name(loc('3'), ident('T')), [])],
        [],
        undefined,
        ReferenceExpression(loc('11'), ident('x'))
      )
    ),
  },
  {
    source: 'f(a) -> x',
    result: Ok(
      FunctionDeclaration(
        Access.Private,
        Name(loc('1'), ident('f')),
        [],
        [FunctionParameter(BindingPattern(loc('3'), ident('a')))],
        undefined,
        ReferenceExpression(loc('9'), ident('x'))
      )
    ),
  },
  {
    source: 'f(): T -> x',
    result: Ok(
      FunctionDeclaration(
        Access.Private,
        Name(loc('1'), ident('f')),
        [],
        [],
        ReferenceType(loc('6'), ident('T')),
        ReferenceExpression(loc('11'), ident('x'))
      )
    ),
  },
  {
    source: 'f(): a -> b -> c',
    result: Ok(
      FunctionDeclaration(
        Access.Private,
        Name(loc('1'), ident('f')),
        [],
        [],
        ReferenceType(loc('6'), ident('a')),
        FunctionExpression(
          loc('11-16'),
          [],
          [FunctionParameter(BindingPattern(loc('11'), ident('b')))],
          ReferenceExpression(loc('16'), ident('c'))
        )
      )
    ),
  },
  {
    source: 'f(): a -> (b -> c)',
    result: Ok(
      FunctionDeclaration(
        Access.Private,
        Name(loc('1'), ident('f')),
        [],
        [],
        ReferenceType(loc('6'), ident('a')),
        WrappedExpression(
          loc('11-18'),
          FunctionExpression(
            loc('12-17'),
            [],
            [FunctionParameter(BindingPattern(loc('12'), ident('b')))],
            ReferenceExpression(loc('17'), ident('c'))
          )
        )
      )
    ),
  },
  {
    source: 'f(): (a -> b) -> c',
    result: Ok(
      FunctionDeclaration(
        Access.Private,
        Name(loc('1'), ident('f')),
        [],
        [],
        WrappedType(
          loc('6-13'),
          FunctionType(
            loc('7-12'),
            [ReferenceType(loc('7'), ident('a'))],
            ReferenceType(loc('12'), ident('b'))
          )
        ),
        ReferenceExpression(loc('18'), ident('c'))
      )
    ),
  },
];

cases.forEach(({source, result}) => {
  test(source.replace(/\n/g, '\\n'), () => {
    expect(parseDeclaration(Lexer.create(source))).toEqual(result);
  });
});
