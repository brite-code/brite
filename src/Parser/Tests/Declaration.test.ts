import {Err, Ok, Result} from '../../Utils/Result';
import {
  Access,
  BindingPattern,
  ClassDeclaration,
  ClassMethod,
  Declaration,
  FunctionDeclaration,
  FunctionExpression,
  FunctionParameter,
  FunctionType,
  GenericType,
  MemberType,
  Name,
  ReferenceExpression,
  ReferenceType,
  TypeDeclaration,
  TypeParameter,
  UnitExpression,
  UnitType,
  WrappedExpression,
  WrappedType,
} from '../Ast';
import {
  ExpectedClassMember,
  ExpectedDeclaration,
  ExpectedGlyph,
  ExpectedIdentifier,
  ExpectedKeyword,
  ParserError,
  UnexpectedTokenError,
} from '../Error';
import {Identifier, ident} from '../Identifier';
import {EndToken, Glyph, GlyphToken, IdentifierToken, Lexer} from '../Lexer';
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
        IdentifierToken(loc('1-4'), ident('data')),
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
        IdentifierToken(loc('1'), ident('f')),
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
          undefined,
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
            undefined,
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
  {
    source: 'type T = ()',
    result: Ok(
      TypeDeclaration(
        Access.Private,
        Name(loc('6'), ident('T')),
        [],
        UnitType(loc('10-11'))
      )
    ),
  },
  {
    source: 'public type T = ()',
    result: Ok(
      TypeDeclaration(
        Access.Public,
        Name(loc('13'), ident('T')),
        [],
        UnitType(loc('17-18'))
      )
    ),
  },
  {
    source: 'type Map<K, V> = Data.Map<K, V>',
    result: Ok(
      TypeDeclaration(
        Access.Private,
        Name(loc('6-8'), ident('Map')),
        [
          TypeParameter(Name(loc('10'), ident('K')), []),
          TypeParameter(Name(loc('13'), ident('V')), []),
        ],
        GenericType(
          loc('18-31'),
          MemberType(
            loc('18-25'),
            ReferenceType(loc('18-21'), ident('Data')),
            Name(loc('23-25'), ident('Map'))
          ),
          [
            ReferenceType(loc('27'), ident('K')),
            ReferenceType(loc('30'), ident('V')),
          ]
        )
      )
    ),
  },
  {
    source: 'class',
    result: Err(UnexpectedTokenError(EndToken(loc('6')), ExpectedIdentifier)),
  },
  {
    source: 'base %',
    result: Err(
      UnexpectedTokenError(
        GlyphToken(loc('6'), Glyph.Percent),
        ExpectedKeyword('class')
      )
    ),
  },
  {
    source: 'base foo',
    result: Err(
      UnexpectedTokenError(
        IdentifierToken(loc('6-8'), ident('foo')),
        ExpectedKeyword('class')
      )
    ),
  },
  {
    source: 'base() -> ()',
    result: Ok(
      FunctionDeclaration(
        Access.Private,
        Name(loc('1-4'), ident('base')),
        [],
        [],
        undefined,
        UnitExpression(loc('11-12'))
      )
    ),
  },
  {
    source: 'unsealed foo',
    result: Err(
      UnexpectedTokenError(
        IdentifierToken(loc('10-12'), ident('foo')),
        ExpectedKeyword('base')
      )
    ),
  },
  {
    source: 'unsealed base foo',
    result: Err(
      UnexpectedTokenError(
        IdentifierToken(loc('15-17'), ident('foo')),
        ExpectedKeyword('class')
      )
    ),
  },
  {
    source: 'class C',
    result: Ok(
      ClassDeclaration({
        access: Access.Private,
        name: Name(loc('7'), ident('C')),
        base: false,
        unsealed: false,
        typeParameters: [],
        parameters: [],
        extends: undefined,
        implements: [],
        body: [],
      })
    ),
  },
  {
    source: 'base class C',
    result: Ok(
      ClassDeclaration({
        access: Access.Private,
        name: Name(loc('12'), ident('C')),
        base: true,
        unsealed: false,
        typeParameters: [],
        parameters: [],
        extends: undefined,
        implements: [],
        body: [],
      })
    ),
  },
  {
    source: 'unsealed base class C',
    result: Ok(
      ClassDeclaration({
        access: Access.Private,
        name: Name(loc('21'), ident('C')),
        base: true,
        unsealed: true,
        typeParameters: [],
        parameters: [],
        extends: undefined,
        implements: [],
        body: [],
      })
    ),
  },
  {
    source: 'unsealed class C',
    result: Err(
      UnexpectedTokenError(
        IdentifierToken(loc('10-14'), ident('class')),
        ExpectedKeyword('base')
      )
    ),
  },
  {
    source: 'base unsealed class C',
    result: Err(
      UnexpectedTokenError(
        IdentifierToken(loc('6-13'), ident('unsealed')),
        ExpectedKeyword('class')
      )
    ),
  },
  {
    source: 'class C<>',
    result: Ok(
      ClassDeclaration({
        access: Access.Private,
        name: Name(loc('7'), ident('C')),
        base: false,
        unsealed: false,
        typeParameters: [],
        parameters: [],
        extends: undefined,
        implements: [],
        body: [],
      })
    ),
  },
  {
    source: 'class C()',
    result: Ok(
      ClassDeclaration({
        access: Access.Private,
        name: Name(loc('7'), ident('C')),
        base: false,
        unsealed: false,
        typeParameters: [],
        parameters: [],
        extends: undefined,
        implements: [],
        body: [],
      })
    ),
  },
  {
    source: 'class C<T>',
    result: Ok(
      ClassDeclaration({
        access: Access.Private,
        name: Name(loc('7'), ident('C')),
        base: false,
        unsealed: false,
        typeParameters: [TypeParameter(Name(loc('9'), ident('T')))],
        parameters: [],
        extends: undefined,
        implements: [],
        body: [],
      })
    ),
  },
  {
    source: 'class C(x)',
    result: Ok(
      ClassDeclaration({
        access: Access.Private,
        name: Name(loc('7'), ident('C')),
        base: false,
        unsealed: false,
        typeParameters: [],
        parameters: [FunctionParameter(BindingPattern(loc('9'), ident('x')))],
        extends: undefined,
        implements: [],
        body: [],
      })
    ),
  },
  {
    source: 'class C<>()',
    result: Ok(
      ClassDeclaration({
        access: Access.Private,
        name: Name(loc('7'), ident('C')),
        base: false,
        unsealed: false,
        typeParameters: [],
        parameters: [],
        extends: undefined,
        implements: [],
        body: [],
      })
    ),
  },
  {
    source: 'class C<T>()',
    result: Ok(
      ClassDeclaration({
        access: Access.Private,
        name: Name(loc('7'), ident('C')),
        base: false,
        unsealed: false,
        typeParameters: [TypeParameter(Name(loc('9'), ident('T')))],
        parameters: [],
        extends: undefined,
        implements: [],
        body: [],
      })
    ),
  },
  {
    source: 'class C<>(x)',
    result: Ok(
      ClassDeclaration({
        access: Access.Private,
        name: Name(loc('7'), ident('C')),
        base: false,
        unsealed: false,
        typeParameters: [],
        parameters: [FunctionParameter(BindingPattern(loc('11'), ident('x')))],
        extends: undefined,
        implements: [],
        body: [],
      })
    ),
  },
  {
    source: 'class C<T>(x)',
    result: Ok(
      ClassDeclaration({
        access: Access.Private,
        name: Name(loc('7'), ident('C')),
        base: false,
        unsealed: false,
        typeParameters: [TypeParameter(Name(loc('9'), ident('T')))],
        parameters: [FunctionParameter(BindingPattern(loc('12'), ident('x')))],
        extends: undefined,
        implements: [],
        body: [],
      })
    ),
  },
  {
    source: 'class C {}',
    result: Ok(
      ClassDeclaration({
        access: Access.Private,
        name: Name(loc('7'), ident('C')),
        base: false,
        unsealed: false,
        typeParameters: [],
        parameters: [],
        extends: undefined,
        implements: [],
        body: [],
      })
    ),
  },
  {
    source: 'class C() {}',
    result: Ok(
      ClassDeclaration({
        access: Access.Private,
        name: Name(loc('7'), ident('C')),
        base: false,
        unsealed: false,
        typeParameters: [],
        parameters: [],
        extends: undefined,
        implements: [],
        body: [],
      })
    ),
  },
  {
    source: 'class C<> {}',
    result: Ok(
      ClassDeclaration({
        access: Access.Private,
        name: Name(loc('7'), ident('C')),
        base: false,
        unsealed: false,
        typeParameters: [],
        parameters: [],
        extends: undefined,
        implements: [],
        body: [],
      })
    ),
  },
  {
    source: 'class C<>() {}',
    result: Ok(
      ClassDeclaration({
        access: Access.Private,
        name: Name(loc('7'), ident('C')),
        base: false,
        unsealed: false,
        typeParameters: [],
        parameters: [],
        extends: undefined,
        implements: [],
        body: [],
      })
    ),
  },
  {
    source: 'class C { public() -> () }',
    result: Ok(
      ClassDeclaration({
        access: Access.Private,
        name: Name(loc('7'), ident('C')),
        base: false,
        unsealed: false,
        typeParameters: [],
        parameters: [],
        extends: undefined,
        implements: [],
        body: [
          ClassMethod({
            access: Access.Private,
            base: false,
            name: Name(loc('11-16'), ident('public')),
            typeParameters: [],
            parameters: [],
            return: undefined,
            body: UnitExpression(loc('23-24')),
          }),
        ],
      })
    ),
  },
  {
    source: 'class C { base public() -> () }',
    result: Ok(
      ClassDeclaration({
        access: Access.Private,
        name: Name(loc('7'), ident('C')),
        base: false,
        unsealed: false,
        typeParameters: [],
        parameters: [],
        extends: undefined,
        implements: [],
        body: [
          ClassMethod({
            access: Access.Private,
            base: true,
            name: Name(loc('16-21'), ident('public')),
            typeParameters: [],
            parameters: [],
            return: undefined,
            body: UnitExpression(loc('28-29')),
          }),
        ],
      })
    ),
  },
  {
    source: 'class C { public base() -> () }',
    result: Ok(
      ClassDeclaration({
        access: Access.Private,
        name: Name(loc('7'), ident('C')),
        base: false,
        unsealed: false,
        typeParameters: [],
        parameters: [],
        extends: undefined,
        implements: [],
        body: [
          ClassMethod({
            access: Access.Public,
            base: false,
            name: Name(loc('18-21'), ident('base')),
            typeParameters: [],
            parameters: [],
            return: undefined,
            body: UnitExpression(loc('28-29')),
          }),
        ],
      })
    ),
  },
  {
    source: 'class C { public base f() -> () }',
    result: Ok(
      ClassDeclaration({
        access: Access.Private,
        name: Name(loc('7'), ident('C')),
        base: false,
        unsealed: false,
        typeParameters: [],
        parameters: [],
        extends: undefined,
        implements: [],
        body: [
          ClassMethod({
            access: Access.Public,
            base: true,
            name: Name(loc('23'), ident('f')),
            typeParameters: [],
            parameters: [],
            return: undefined,
            body: UnitExpression(loc('30-31')),
          }),
        ],
      })
    ),
  },
  {
    source: 'class C { base public f() -> () }',
    result: Err(
      UnexpectedTokenError(
        IdentifierToken(loc('16-21'), ident('public')),
        ExpectedClassMember
      )
    ),
  },
  {
    source: 'class C { public f() -> () }',
    result: Ok(
      ClassDeclaration({
        access: Access.Private,
        name: Name(loc('7'), ident('C')),
        base: false,
        unsealed: false,
        typeParameters: [],
        parameters: [],
        extends: undefined,
        implements: [],
        body: [
          ClassMethod({
            access: Access.Public,
            base: false,
            name: Name(loc('18'), ident('f')),
            typeParameters: [],
            parameters: [],
            return: undefined,
            body: UnitExpression(loc('25-26')),
          }),
        ],
      })
    ),
  },
  {
    source: 'class C { base f() -> () }',
    result: Ok(
      ClassDeclaration({
        access: Access.Private,
        name: Name(loc('7'), ident('C')),
        base: false,
        unsealed: false,
        typeParameters: [],
        parameters: [],
        extends: undefined,
        implements: [],
        body: [
          ClassMethod({
            access: Access.Private,
            base: true,
            name: Name(loc('16'), ident('f')),
            typeParameters: [],
            parameters: [],
            return: undefined,
            body: UnitExpression(loc('23-24')),
          }),
        ],
      })
    ),
  },
  {
    source: 'class C { public<>() -> () }',
    result: Ok(
      ClassDeclaration({
        access: Access.Private,
        name: Name(loc('7'), ident('C')),
        base: false,
        unsealed: false,
        typeParameters: [],
        parameters: [],
        extends: undefined,
        implements: [],
        body: [
          ClassMethod({
            access: Access.Private,
            base: false,
            name: Name(loc('11-16'), ident('public')),
            typeParameters: [],
            parameters: [],
            return: undefined,
            body: UnitExpression(loc('25-26')),
          }),
        ],
      })
    ),
  },
  {
    source: 'class C { base public<>() -> () }',
    result: Ok(
      ClassDeclaration({
        access: Access.Private,
        name: Name(loc('7'), ident('C')),
        base: false,
        unsealed: false,
        typeParameters: [],
        parameters: [],
        extends: undefined,
        implements: [],
        body: [
          ClassMethod({
            access: Access.Private,
            base: true,
            name: Name(loc('16-21'), ident('public')),
            typeParameters: [],
            parameters: [],
            return: undefined,
            body: UnitExpression(loc('30-31')),
          }),
        ],
      })
    ),
  },
  {
    source: 'class C { public base<>() -> () }',
    result: Ok(
      ClassDeclaration({
        access: Access.Private,
        name: Name(loc('7'), ident('C')),
        base: false,
        unsealed: false,
        typeParameters: [],
        parameters: [],
        extends: undefined,
        implements: [],
        body: [
          ClassMethod({
            access: Access.Public,
            base: false,
            name: Name(loc('18-21'), ident('base')),
            typeParameters: [],
            parameters: [],
            return: undefined,
            body: UnitExpression(loc('30-31')),
          }),
        ],
      })
    ),
  },
  {
    source: 'class C { public base f<>() -> () }',
    result: Ok(
      ClassDeclaration({
        access: Access.Private,
        name: Name(loc('7'), ident('C')),
        base: false,
        unsealed: false,
        typeParameters: [],
        parameters: [],
        extends: undefined,
        implements: [],
        body: [
          ClassMethod({
            access: Access.Public,
            base: true,
            name: Name(loc('23'), ident('f')),
            typeParameters: [],
            parameters: [],
            return: undefined,
            body: UnitExpression(loc('32-33')),
          }),
        ],
      })
    ),
  },
  {
    source: 'class C { public f<>() -> () }',
    result: Ok(
      ClassDeclaration({
        access: Access.Private,
        name: Name(loc('7'), ident('C')),
        base: false,
        unsealed: false,
        typeParameters: [],
        parameters: [],
        extends: undefined,
        implements: [],
        body: [
          ClassMethod({
            access: Access.Public,
            base: false,
            name: Name(loc('18'), ident('f')),
            typeParameters: [],
            parameters: [],
            return: undefined,
            body: UnitExpression(loc('27-28')),
          }),
        ],
      })
    ),
  },
  {
    source: 'class C { base f<>() -> () }',
    result: Ok(
      ClassDeclaration({
        access: Access.Private,
        name: Name(loc('7'), ident('C')),
        base: false,
        unsealed: false,
        typeParameters: [],
        parameters: [],
        extends: undefined,
        implements: [],
        body: [
          ClassMethod({
            access: Access.Private,
            base: true,
            name: Name(loc('16'), ident('f')),
            typeParameters: [],
            parameters: [],
            return: undefined,
            body: UnitExpression(loc('25-26')),
          }),
        ],
      })
    ),
  },
  {
    source: 'class C { f % }',
    result: Err(
      UnexpectedTokenError(
        IdentifierToken(loc('11'), ident('f')),
        ExpectedClassMember
      )
    ),
  },
  {
    source: 'class C { public % }',
    result: Err(
      UnexpectedTokenError(
        GlyphToken(loc('18'), Glyph.Percent),
        ExpectedClassMember
      )
    ),
  },
  {
    source: 'class C { base % }',
    result: Err(
      UnexpectedTokenError(
        GlyphToken(loc('16'), Glyph.Percent),
        ExpectedClassMember
      )
    ),
  },
  {
    source: 'class C { public base % }',
    result: Err(
      UnexpectedTokenError(
        GlyphToken(loc('23'), Glyph.Percent),
        ExpectedClassMember
      )
    ),
  },
  {
    source: 'class C { base f(): T -> () }',
    result: Ok(
      ClassDeclaration({
        access: Access.Private,
        name: Name(loc('7'), ident('C')),
        base: false,
        unsealed: false,
        typeParameters: [],
        parameters: [],
        extends: undefined,
        implements: [],
        body: [
          ClassMethod({
            access: Access.Private,
            base: true,
            name: Name(loc('16'), ident('f')),
            typeParameters: [],
            parameters: [],
            return: ReferenceType(loc('21'), ident('T')),
            body: UnitExpression(loc('26-27')),
          }),
        ],
      })
    ),
  },
  {
    source: 'class C { base f(): T }',
    result: Ok(
      ClassDeclaration({
        access: Access.Private,
        name: Name(loc('7'), ident('C')),
        base: false,
        unsealed: false,
        typeParameters: [],
        parameters: [],
        extends: undefined,
        implements: [],
        body: [
          ClassMethod({
            access: Access.Private,
            base: true,
            name: Name(loc('16'), ident('f')),
            typeParameters: [],
            parameters: [],
            return: ReferenceType(loc('21'), ident('T')),
            body: undefined,
          }),
        ],
      })
    ),
  },
  {
    source: 'class C { base f() }',
    result: Err(
      UnexpectedTokenError(
        GlyphToken(loc('20'), Glyph.BraceRight),
        ExpectedGlyph(Glyph.Arrow)
      )
    ),
  },
  {
    source: 'class C { f(): T }',
    result: Ok(
      ClassDeclaration({
        access: Access.Private,
        name: Name(loc('7'), ident('C')),
        base: false,
        unsealed: false,
        typeParameters: [],
        parameters: [],
        extends: undefined,
        implements: [],
        body: [
          ClassMethod({
            access: Access.Private,
            base: false,
            name: Name(loc('11'), ident('f')),
            typeParameters: [],
            parameters: [],
            return: ReferenceType(loc('16'), ident('T')),
            body: undefined,
          }),
        ],
      })
    ),
  },
  {
    source: 'class C { f() }',
    result: Err(
      UnexpectedTokenError(
        GlyphToken(loc('15'), Glyph.BraceRight),
        ExpectedGlyph(Glyph.Arrow)
      )
    ),
  },
  {
    source: 'class C extends P',
    result: Ok(
      ClassDeclaration({
        access: Access.Private,
        name: Name(loc('7'), ident('C')),
        base: false,
        unsealed: false,
        typeParameters: [],
        parameters: [],
        extends: ReferenceType(loc('17'), ident('P')),
        implements: [],
        body: [],
      })
    ),
  },
  {
    source: 'class C<T> extends P<T>',
    result: Ok(
      ClassDeclaration({
        access: Access.Private,
        name: Name(loc('7'), ident('C')),
        base: false,
        unsealed: false,
        typeParameters: [TypeParameter(Name(loc('9'), ident('T')))],
        parameters: [],
        extends: GenericType(
          loc('20-23'),
          ReferenceType(loc('20'), ident('P')),
          [ReferenceType(loc('22'), ident('T'))]
        ),
        implements: [],
        body: [],
      })
    ),
  },
];

cases.forEach(({source, result}) => {
  test(source.replace(/\n/g, '\\n'), () => {
    expect(parseDeclaration(Lexer.create(source))).toEqual(result);
  });
});
