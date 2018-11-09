import {Err, Ok} from '../../Utils/Result';
import {
  FunctionType,
  GenericType,
  MemberType,
  Name,
  RecordType,
  RecordTypeProperty,
  ReferenceType,
  TupleType,
  UnitType,
  WrappedType,
} from '../Ast';
import {ExpectedIdentifier, ExpectedType, UnexpectedTokenError} from '../Error';
import {Identifier, ident} from '../Identifier';
import {EndToken, Glyph, GlyphToken, IdentifierToken, Lexer} from '../Lexer';
import {loc} from '../Loc';
import {parseType} from '../Parser';

[
  {
    source: '',
    result: Err(UnexpectedTokenError(EndToken(loc('1')), ExpectedType)),
  },
  {
    source: '...',
    result: Err(
      UnexpectedTokenError(GlyphToken(loc('1-3'), Glyph.Ellipsis), ExpectedType)
    ),
  },
  {
    source: 'in',
    result: Err(
      UnexpectedTokenError(
        IdentifierToken(loc('1-2'), 'in' as Identifier),
        ExpectedType
      )
    ),
  },
  {
    source: 'foo',
    result: Ok(ReferenceType(loc('1-3'), ident('foo'))),
  },
  {source: '()', result: Ok(UnitType(loc('1-2')))},
  {source: '( )', result: Ok(UnitType(loc('1-3')))},
  {
    source: '(,)',
    result: Err(
      UnexpectedTokenError(GlyphToken(loc('2'), Glyph.Comma), ExpectedType)
    ),
  },
  {
    source: '(foo)',
    result: Ok(
      WrappedType(loc('1-5'), ReferenceType(loc('2-4'), ident('foo')))
    ),
  },
  {
    source: '(foo,)',
    result: Ok(
      WrappedType(loc('1-6'), ReferenceType(loc('2-4'), ident('foo')))
    ),
  },
  {
    source: '(foo, bar)',
    result: Ok(
      TupleType(loc('1-10'), [
        ReferenceType(loc('2-4'), ident('foo')),
        ReferenceType(loc('7-9'), ident('bar')),
      ])
    ),
  },
  {
    source: '(foo, bar, qux)',
    result: Ok(
      TupleType(loc('1-15'), [
        ReferenceType(loc('2-4'), ident('foo')),
        ReferenceType(loc('7-9'), ident('bar')),
        ReferenceType(loc('12-14'), ident('qux')),
      ])
    ),
  },
  {
    source: '(foo, bar, qux, lit)',
    result: Ok(
      TupleType(loc('1-20'), [
        ReferenceType(loc('2-4'), ident('foo')),
        ReferenceType(loc('7-9'), ident('bar')),
        ReferenceType(loc('12-14'), ident('qux')),
        ReferenceType(loc('17-19'), ident('lit')),
      ])
    ),
  },
  {source: '{}', result: Ok(RecordType(loc('1-2'), []))},
  {
    source: '{ foo: T }',
    result: Ok(
      RecordType(loc('1-10'), [
        RecordTypeProperty(
          Name(loc('3-5'), ident('foo')),
          ReferenceType(loc('8'), ident('T'))
        ),
      ])
    ),
  },
  {
    source: '{ foo: T, bar: U }',
    result: Ok(
      RecordType(loc('1-18'), [
        RecordTypeProperty(
          Name(loc('3-5'), ident('foo')),
          ReferenceType(loc('8'), ident('T'))
        ),
        RecordTypeProperty(
          Name(loc('11-13'), ident('bar')),
          ReferenceType(loc('16'), ident('U'))
        ),
      ])
    ),
  },
  {
    source: '{ foo: T, bar: U, qux: V, lit: W }',
    result: Ok(
      RecordType(loc('1-34'), [
        RecordTypeProperty(
          Name(loc('3-5'), ident('foo')),
          ReferenceType(loc('8'), ident('T'))
        ),
        RecordTypeProperty(
          Name(loc('11-13'), ident('bar')),
          ReferenceType(loc('16'), ident('U'))
        ),
        RecordTypeProperty(
          Name(loc('19-21'), ident('qux')),
          ReferenceType(loc('24'), ident('V'))
        ),
        RecordTypeProperty(
          Name(loc('27-29'), ident('lit')),
          ReferenceType(loc('32'), ident('W'))
        ),
      ])
    ),
  },
  {
    source: '{ foo?: T }',
    result: Ok(
      RecordType(loc('1-11'), [
        RecordTypeProperty(
          Name(loc('3-5'), ident('foo')),
          ReferenceType(loc('9'), ident('T')),
          {optional: true}
        ),
      ])
    ),
  },
  {
    source: 'hello.world',
    result: Ok(
      MemberType(
        loc('1-11'),
        ReferenceType(loc('1-5'), ident('hello')),
        Name(loc('7-11'), ident('world'))
      )
    ),
  },
  {
    source: 'foo.bar.qux',
    result: Ok(
      MemberType(
        loc('1-11'),
        MemberType(
          loc('1-7'),
          ReferenceType(loc('1-3'), ident('foo')),
          Name(loc('5-7'), ident('bar'))
        ),
        Name(loc('9-11'), ident('qux'))
      )
    ),
  },
  {
    source: 'foo.bar.qux.lit',
    result: Ok(
      MemberType(
        loc('1-15'),
        MemberType(
          loc('1-11'),
          MemberType(
            loc('1-7'),
            ReferenceType(loc('1-3'), ident('foo')),
            Name(loc('5-7'), ident('bar'))
          ),
          Name(loc('9-11'), ident('qux'))
        ),
        Name(loc('13-15'), ident('lit'))
      )
    ),
  },
  {
    source: 'hello.',
    result: Err(UnexpectedTokenError(EndToken(loc('7')), ExpectedIdentifier)),
  },
  {
    source: 'hello.%',
    result: Err(
      UnexpectedTokenError(
        GlyphToken(loc('7'), Glyph.Percent),
        ExpectedIdentifier
      )
    ),
  },
  {
    source: 'T<>',
    result: Ok(
      GenericType(loc('1-3'), ReferenceType(loc('1'), ident('T')), [])
    ),
  },
  {
    source: 'T<A>',
    result: Ok(
      GenericType(loc('1-4'), ReferenceType(loc('1'), ident('T')), [
        ReferenceType(loc('3'), ident('A')),
      ])
    ),
  },
  {
    source: 'T<A, B>',
    result: Ok(
      GenericType(loc('1-7'), ReferenceType(loc('1'), ident('T')), [
        ReferenceType(loc('3'), ident('A')),
        ReferenceType(loc('6'), ident('B')),
      ])
    ),
  },
  {
    source: 'T<A, B, C, D>',
    result: Ok(
      GenericType(loc('1-13'), ReferenceType(loc('1'), ident('T')), [
        ReferenceType(loc('3'), ident('A')),
        ReferenceType(loc('6'), ident('B')),
        ReferenceType(loc('9'), ident('C')),
        ReferenceType(loc('12'), ident('D')),
      ])
    ),
  },
  {
    source: 'React.Component<Props>',
    result: Ok(
      GenericType(
        loc('1-22'),
        MemberType(
          loc('1-15'),
          ReferenceType(loc('1-5'), ident('React')),
          Name(loc('7-15'), ident('Component'))
        ),
        [ReferenceType(loc('17-21'), ident('Props'))]
      )
    ),
  },
  {
    source: 'A<>.B',
    result: Ok(
      MemberType(
        loc('1-5'),
        GenericType(loc('1-3'), ReferenceType(loc('1'), ident('A')), []),
        Name(loc('5'), ident('B'))
      )
    ),
  },
  {
    source: 'A<><>',
    result: Ok(
      GenericType(
        loc('1-5'),
        GenericType(loc('1-3'), ReferenceType(loc('1'), ident('A')), []),
        []
      )
    ),
  },
  {
    source: '().foo',
    result: Ok(
      MemberType(
        loc('1-6'),
        UnitType(loc('1-2')),
        Name(loc('4-6'), ident('foo'))
      )
    ),
  },
  {
    source: '{}.foo',
    result: Ok(
      MemberType(
        loc('1-6'),
        RecordType(loc('1-2'), []),
        Name(loc('4-6'), ident('foo'))
      )
    ),
  },
  {
    source: '() -> ()',
    result: Ok(FunctionType(loc('1-8'), [], UnitType(loc('7-8')))),
  },
  {
    source: '(a) -> ()',
    result: Ok(
      FunctionType(
        loc('1-9'),
        [ReferenceType(loc('2'), ident('a'))],
        UnitType(loc('8-9'))
      )
    ),
  },
  {
    source: '(a, b) -> ()',
    result: Ok(
      FunctionType(
        loc('1-12'),
        [
          ReferenceType(loc('2'), ident('a')),
          ReferenceType(loc('5'), ident('b')),
        ],
        UnitType(loc('11-12'))
      )
    ),
  },
  {
    source: '(a, b, c, d) -> ()',
    result: Ok(
      FunctionType(
        loc('1-18'),
        [
          ReferenceType(loc('2'), ident('a')),
          ReferenceType(loc('5'), ident('b')),
          ReferenceType(loc('8'), ident('c')),
          ReferenceType(loc('11'), ident('d')),
        ],
        UnitType(loc('17-18'))
      )
    ),
  },
  {
    source: '() -> foo.bar',
    result: Ok(
      FunctionType(
        loc('1-13'),
        [],
        MemberType(
          loc('7-13'),
          ReferenceType(loc('7-9'), ident('foo')),
          Name(loc('11-13'), ident('bar'))
        )
      )
    ),
  },
  {
    source: '() ->',
    result: Err(UnexpectedTokenError(EndToken(loc('6')), ExpectedType)),
  },
  {
    source: 'T -> T',
    result: Ok(
      FunctionType(
        loc('1-6'),
        [ReferenceType(loc('1'), ident('T'))],
        ReferenceType(loc('6'), ident('T'))
      )
    ),
  },
  {
    source: 'x -> foo.bar',
    result: Ok(
      FunctionType(
        loc('1-12'),
        [ReferenceType(loc('1'), ident('x'))],
        MemberType(
          loc('6-12'),
          ReferenceType(loc('6-8'), ident('foo')),
          Name(loc('10-12'), ident('bar'))
        )
      )
    ),
  },
  {
    source: '(X, Y, %, Z)',
    result: Err(
      UnexpectedTokenError(GlyphToken(loc('8'), Glyph.Percent), ExpectedType)
    ),
  },
  {
    source: 'T<X, Y, %, Z>',
    result: Err(
      UnexpectedTokenError(GlyphToken(loc('9'), Glyph.Percent), ExpectedType)
    ),
  },
  {
    source: '<T> T',
    result: Err(
      UnexpectedTokenError(GlyphToken(loc('1'), Glyph.LessThan), ExpectedType)
    ),
  },
  {
    source: '<T> Foo<T>',
    result: Err(
      UnexpectedTokenError(GlyphToken(loc('1'), Glyph.LessThan), ExpectedType)
    ),
  },
  {
    source: '<T>(T) -> T',
    result: Err(
      UnexpectedTokenError(GlyphToken(loc('1'), Glyph.LessThan), ExpectedType)
    ),
  },
  {
    source: '<T> T -> T',
    result: Err(
      UnexpectedTokenError(GlyphToken(loc('1'), Glyph.LessThan), ExpectedType)
    ),
  },
  {
    source: '<T> %',
    result: Err(
      UnexpectedTokenError(GlyphToken(loc('1'), Glyph.LessThan), ExpectedType)
    ),
  },
  {
    source: '<T: A> T',
    result: Err(
      UnexpectedTokenError(GlyphToken(loc('1'), Glyph.LessThan), ExpectedType)
    ),
  },
  {
    source: '<T: A + B> T',
    result: Err(
      UnexpectedTokenError(GlyphToken(loc('1'), Glyph.LessThan), ExpectedType)
    ),
  },
  {
    source: '<T: A + B + C + D> T',
    result: Err(
      UnexpectedTokenError(GlyphToken(loc('1'), Glyph.LessThan), ExpectedType)
    ),
  },
  {
    source: '<T: > T',
    result: Err(
      UnexpectedTokenError(GlyphToken(loc('1'), Glyph.LessThan), ExpectedType)
    ),
  },
  {
    source: '<T: %> T',
    result: Err(
      UnexpectedTokenError(GlyphToken(loc('1'), Glyph.LessThan), ExpectedType)
    ),
  },
  {
    source: '<T: A + %> T',
    result: Err(
      UnexpectedTokenError(GlyphToken(loc('1'), Glyph.LessThan), ExpectedType)
    ),
  },
  {
    source: '<T: A B> T',
    result: Err(
      UnexpectedTokenError(GlyphToken(loc('1'), Glyph.LessThan), ExpectedType)
    ),
  },
].forEach(({source, result}) => {
  test(source, () => {
    expect(parseType(Lexer.create(source))).toEqual(result);
  });
});
