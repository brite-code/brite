import {Err, Ok} from '../Utils/Result';

import {
  BindingPattern,
  FunctionType,
  GenericType,
  HolePattern,
  ListPattern,
  MemberType,
  Name,
  QuantifiedType,
  RecordPattern,
  RecordPatternProperty,
  RecordType,
  RecordTypeProperty,
  ReferenceType,
  TuplePattern,
  TuplePatternElement,
  TupleType,
  TypeParameter,
  UnitPattern,
  UnitType,
  WrappedPattern,
  WrappedType,
} from './Ast';
import {
  ExpectedBindingIdentifier,
  ExpectedGlyph,
  ExpectedIdentifier,
  ExpectedPattern,
  ExpectedType,
  UnexpectedTokenError,
} from './Error';
import {BindingIdentifier, Identifier, ident} from './Identifier';
import {EndToken, Glyph, GlyphToken, IdentifierToken, Lexer} from './Lexer';
import {loc} from './Loc';
import {parseCommaListTest, parsePattern, parseType} from './Parser';

function lex(source: string): Lexer {
  return Lexer.create(source);
}

describe('type', () => {
  [
    {
      source: '',
      result: Err(UnexpectedTokenError(EndToken(loc('1')), ExpectedType)),
    },
    {
      source: '...',
      result: Err(
        UnexpectedTokenError(
          GlyphToken(loc('1-3'), Glyph.Ellipsis),
          ExpectedType
        )
      ),
    },
    {
      source: 'if',
      result: Err(
        UnexpectedTokenError(
          IdentifierToken(loc('1-2'), 'if' as Identifier),
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
      result: Ok(
        QuantifiedType(
          loc('1-5'),
          [TypeParameter(Name(loc('2'), ident('T')), [])],
          ReferenceType(loc('5'), ident('T'))
        )
      ),
    },
    {
      source: '<T> Foo<T>',
      result: Ok(
        QuantifiedType(
          loc('1-10'),
          [TypeParameter(Name(loc('2'), ident('T')), [])],
          GenericType(loc('5-10'), ReferenceType(loc('5-7'), ident('Foo')), [
            ReferenceType(loc('9'), ident('T')),
          ])
        )
      ),
    },
    {
      source: '<T>(T) -> T',
      result: Ok(
        QuantifiedType(
          loc('1-11'),
          [TypeParameter(Name(loc('2'), ident('T')), [])],
          FunctionType(
            loc('4-11'),
            [ReferenceType(loc('5'), ident('T'))],
            ReferenceType(loc('11'), ident('T'))
          )
        )
      ),
    },
    {
      source: '<T> T -> T',
      result: Ok(
        QuantifiedType(
          loc('1-10'),
          [TypeParameter(Name(loc('2'), ident('T')), [])],
          FunctionType(
            loc('5-10'),
            [ReferenceType(loc('5'), ident('T'))],
            ReferenceType(loc('10'), ident('T'))
          )
        )
      ),
    },
    {
      source: '<T> %',
      result: Err(
        UnexpectedTokenError(GlyphToken(loc('5'), Glyph.Percent), ExpectedType)
      ),
    },
    {
      source: '<T: A> T',
      result: Ok(
        QuantifiedType(
          loc('1-8'),
          [
            TypeParameter(Name(loc('2'), ident('T')), [
              ReferenceType(loc('5'), ident('A')),
            ]),
          ],
          ReferenceType(loc('8'), ident('T'))
        )
      ),
    },
    {
      source: '<T: A + B> T',
      result: Ok(
        QuantifiedType(
          loc('1-12'),
          [
            TypeParameter(Name(loc('2'), ident('T')), [
              ReferenceType(loc('5'), ident('A')),
              ReferenceType(loc('9'), ident('B')),
            ]),
          ],
          ReferenceType(loc('12'), ident('T'))
        )
      ),
    },
    {
      source: '<T: A + B + C + D> T',
      result: Ok(
        QuantifiedType(
          loc('1-20'),
          [
            TypeParameter(Name(loc('2'), ident('T')), [
              ReferenceType(loc('5'), ident('A')),
              ReferenceType(loc('9'), ident('B')),
              ReferenceType(loc('13'), ident('C')),
              ReferenceType(loc('17'), ident('D')),
            ]),
          ],
          ReferenceType(loc('20'), ident('T'))
        )
      ),
    },
    {
      source: '<T: > T',
      result: Err(
        UnexpectedTokenError(
          GlyphToken(loc('5'), Glyph.GreaterThan),
          ExpectedType
        )
      ),
    },
    {
      source: '<T: %> T',
      result: Err(
        UnexpectedTokenError(GlyphToken(loc('5'), Glyph.Percent), ExpectedType)
      ),
    },
    {
      source: '<T: A + %> T',
      result: Err(
        UnexpectedTokenError(GlyphToken(loc('9'), Glyph.Percent), ExpectedType)
      ),
    },
    {
      source: '<T: A B> T',
      result: Err(
        UnexpectedTokenError(
          IdentifierToken(loc('7'), ident('B')),
          ExpectedGlyph(Glyph.Comma)
        )
      ),
    },
  ].forEach(({source, result}) => {
    test(source, () => {
      expect(parseType(lex(source))).toEqual(result);
    });
  });
});

describe('pattern', () => {
  [
    {
      source: 'foo',
      result: Ok(BindingPattern(loc('1-3'), ident('foo'))),
    },
    {
      source: 'if',
      result: Err(
        UnexpectedTokenError(
          IdentifierToken(loc('1-2'), 'if' as Identifier),
          ExpectedPattern
        )
      ),
    },
    {source: '_', result: Ok(HolePattern(loc('1')))},
    {
      source: '_x',
      result: Ok(BindingPattern(loc('1-2'), ident('_x'))),
    },
    {source: '()', result: Ok(UnitPattern(loc('1-2')))},
    {
      source: '(foo)',
      result: Ok(
        WrappedPattern(
          loc('1-5'),
          BindingPattern(loc('2-4'), ident('foo')),
          undefined
        )
      ),
    },
    {
      source: '(a, b)',
      result: Ok(
        TuplePattern(loc('1-6'), [
          TuplePatternElement(BindingPattern(loc('2'), ident('a')), undefined),
          TuplePatternElement(BindingPattern(loc('5'), ident('b')), undefined),
        ])
      ),
    },
    {
      source: '(a, b, c, d)',
      result: Ok(
        TuplePattern(loc('1-12'), [
          TuplePatternElement(BindingPattern(loc('2'), ident('a')), undefined),
          TuplePatternElement(BindingPattern(loc('5'), ident('b')), undefined),
          TuplePatternElement(BindingPattern(loc('8'), ident('c')), undefined),
          TuplePatternElement(BindingPattern(loc('11'), ident('d')), undefined),
        ])
      ),
    },
    {
      source: '(,)',
      result: Err(
        UnexpectedTokenError(GlyphToken(loc('2'), Glyph.Comma), ExpectedPattern)
      ),
    },
    {
      source: '(foo,)',
      result: Ok(
        WrappedPattern(
          loc('1-6'),
          BindingPattern(loc('2-4'), ident('foo')),
          undefined
        )
      ),
    },
    {
      source: '(a, b,)',
      result: Ok(
        TuplePattern(loc('1-7'), [
          TuplePatternElement(BindingPattern(loc('2'), ident('a')), undefined),
          TuplePatternElement(BindingPattern(loc('5'), ident('b')), undefined),
        ])
      ),
    },
    {
      source: '(a, b, c, d,)',
      result: Ok(
        TuplePattern(loc('1-13'), [
          TuplePatternElement(BindingPattern(loc('2'), ident('a')), undefined),
          TuplePatternElement(BindingPattern(loc('5'), ident('b')), undefined),
          TuplePatternElement(BindingPattern(loc('8'), ident('c')), undefined),
          TuplePatternElement(BindingPattern(loc('11'), ident('d')), undefined),
        ])
      ),
    },
    {
      source: '(x:)',
      result: Err(
        UnexpectedTokenError(
          GlyphToken(loc('4'), Glyph.ParenRight),
          ExpectedType
        )
      ),
    },
    {
      source: '(x: T)',
      result: Ok(
        WrappedPattern(
          loc('1-6'),
          BindingPattern(loc('2'), ident('x')),
          ReferenceType(loc('5'), ident('T'))
        )
      ),
    },
    {
      source: '(a: A, b: B)',
      result: Ok(
        TuplePattern(loc('1-12'), [
          TuplePatternElement(
            BindingPattern(loc('2'), ident('a')),
            ReferenceType(loc('5'), ident('A'))
          ),
          TuplePatternElement(
            BindingPattern(loc('8'), ident('b')),
            ReferenceType(loc('11'), ident('B'))
          ),
        ])
      ),
    },
    {
      source: '(a: A, b: B, c: C, d: D)',
      result: Ok(
        TuplePattern(loc('1-24'), [
          TuplePatternElement(
            BindingPattern(loc('2'), ident('a')),
            ReferenceType(loc('5'), ident('A'))
          ),
          TuplePatternElement(
            BindingPattern(loc('8'), ident('b')),
            ReferenceType(loc('11'), ident('B'))
          ),
          TuplePatternElement(
            BindingPattern(loc('14'), ident('c')),
            ReferenceType(loc('17'), ident('C'))
          ),
          TuplePatternElement(
            BindingPattern(loc('20'), ident('d')),
            ReferenceType(loc('23'), ident('D'))
          ),
        ])
      ),
    },
    {
      source: '(x: T,)',
      result: Ok(
        WrappedPattern(
          loc('1-7'),
          BindingPattern(loc('2'), ident('x')),
          ReferenceType(loc('5'), ident('T'))
        )
      ),
    },
    {
      source: '(a: A, b: B,)',
      result: Ok(
        TuplePattern(loc('1-13'), [
          TuplePatternElement(
            BindingPattern(loc('2'), ident('a')),
            ReferenceType(loc('5'), ident('A'))
          ),
          TuplePatternElement(
            BindingPattern(loc('8'), ident('b')),
            ReferenceType(loc('11'), ident('B'))
          ),
        ])
      ),
    },
    {source: '{}', result: Ok(RecordPattern(loc('1-2'), []))},
    {
      source: '{ foo }',
      result: Ok(
        RecordPattern(loc('1-7'), [
          RecordPatternProperty(
            Name(loc('3-5'), ident('foo')),
            BindingPattern(loc('3-5'), ident('foo')),
            undefined
          ),
        ])
      ),
    },
    {
      source: '{ foo, bar }',
      result: Ok(
        RecordPattern(loc('1-12'), [
          RecordPatternProperty(
            Name(loc('3-5'), ident('foo')),
            BindingPattern(loc('3-5'), ident('foo')),
            undefined
          ),
          RecordPatternProperty(
            Name(loc('8-10'), ident('bar')),
            BindingPattern(loc('8-10'), ident('bar')),
            undefined
          ),
        ])
      ),
    },
    {
      source: '{ foo, bar, qux, lit }',
      result: Ok(
        RecordPattern(loc('1-22'), [
          RecordPatternProperty(
            Name(loc('3-5'), ident('foo')),
            BindingPattern(loc('3-5'), ident('foo')),
            undefined
          ),
          RecordPatternProperty(
            Name(loc('8-10'), ident('bar')),
            BindingPattern(loc('8-10'), ident('bar')),
            undefined
          ),
          RecordPatternProperty(
            Name(loc('13-15'), ident('qux')),
            BindingPattern(loc('13-15'), ident('qux')),
            undefined
          ),
          RecordPatternProperty(
            Name(loc('18-20'), ident('lit')),
            BindingPattern(loc('18-20'), ident('lit')),
            undefined
          ),
        ])
      ),
    },
    {
      source: '{ foo: T }',
      result: Ok(
        RecordPattern(loc('1-10'), [
          RecordPatternProperty(
            Name(loc('3-5'), ident('foo')),
            BindingPattern(loc('3-5'), ident('foo')),
            ReferenceType(loc('8'), ident('T'))
          ),
        ])
      ),
    },
    {
      source: '{ foo: T, bar: U }',
      result: Ok(
        RecordPattern(loc('1-18'), [
          RecordPatternProperty(
            Name(loc('3-5'), ident('foo')),
            BindingPattern(loc('3-5'), ident('foo')),
            ReferenceType(loc('8'), ident('T'))
          ),
          RecordPatternProperty(
            Name(loc('11-13'), ident('bar')),
            BindingPattern(loc('11-13'), ident('bar')),
            ReferenceType(loc('16'), ident('U'))
          ),
        ])
      ),
    },
    {
      source: '{ foo: T, bar: U, qux: V, lit: W }',
      result: Ok(
        RecordPattern(loc('1-34'), [
          RecordPatternProperty(
            Name(loc('3-5'), ident('foo')),
            BindingPattern(loc('3-5'), ident('foo')),
            ReferenceType(loc('8'), ident('T'))
          ),
          RecordPatternProperty(
            Name(loc('11-13'), ident('bar')),
            BindingPattern(loc('11-13'), ident('bar')),
            ReferenceType(loc('16'), ident('U'))
          ),
          RecordPatternProperty(
            Name(loc('19-21'), ident('qux')),
            BindingPattern(loc('19-21'), ident('qux')),
            ReferenceType(loc('24'), ident('V'))
          ),
          RecordPatternProperty(
            Name(loc('27-29'), ident('lit')),
            BindingPattern(loc('27-29'), ident('lit')),
            ReferenceType(loc('32'), ident('W'))
          ),
        ])
      ),
    },
    {
      source: '{ foo = foo2 }',
      result: Ok(
        RecordPattern(loc('1-14'), [
          RecordPatternProperty(
            Name(loc('3-5'), ident('foo')),
            BindingPattern(loc('9-12'), ident('foo2')),
            undefined
          ),
        ])
      ),
    },
    {
      source: '{ foo = foo2, bar = bar2 }',
      result: Ok(
        RecordPattern(loc('1-26'), [
          RecordPatternProperty(
            Name(loc('3-5'), ident('foo')),
            BindingPattern(loc('9-12'), ident('foo2')),
            undefined
          ),
          RecordPatternProperty(
            Name(loc('15-17'), ident('bar')),
            BindingPattern(loc('21-24'), ident('bar2')),
            undefined
          ),
        ])
      ),
    },
    {
      source: '{ foo: T = x }',
      result: Ok(
        RecordPattern(loc('1-14'), [
          RecordPatternProperty(
            Name(loc('3-5'), ident('foo')),
            BindingPattern(loc('12'), ident('x')),
            ReferenceType(loc('8'), ident('T'))
          ),
        ])
      ),
    },
    {
      source: '{ foo?: T }',
      result: Ok(
        RecordPattern(loc('1-11'), [
          RecordPatternProperty(
            Name(loc('3-5'), ident('foo')),
            BindingPattern(loc('3-5'), ident('foo')),
            ReferenceType(loc('9'), ident('T')),
            {optional: true}
          ),
        ])
      ),
    },
    {
      source: '{ foo? }',
      result: Err(
        UnexpectedTokenError(
          GlyphToken(loc('8'), Glyph.BraceRight),
          ExpectedGlyph(Glyph.Colon)
        )
      ),
    },
    {
      source: "{ if = if' }",
      result: Ok(
        RecordPattern(loc('1-12'), [
          RecordPatternProperty(
            Name(loc('3-4'), 'if' as BindingIdentifier),
            BindingPattern(loc('8-10'), ident("if'")),
            undefined
          ),
        ])
      ),
    },
    {
      source: '{ if }',
      result: Err(
        UnexpectedTokenError(
          IdentifierToken(loc('3-4'), 'if' as Identifier),
          ExpectedBindingIdentifier
        )
      ),
    },
    {
      source: '[]',
      result: Ok(ListPattern(loc('1-2'), [])),
    },
    {
      source: '[a]',
      result: Ok(
        ListPattern(loc('1-3'), [BindingPattern(loc('2'), ident('a'))])
      ),
    },
    {
      source: '[a, b]',
      result: Ok(
        ListPattern(loc('1-6'), [
          BindingPattern(loc('2'), ident('a')),
          BindingPattern(loc('5'), ident('b')),
        ])
      ),
    },
    {
      source: '[a, b, c, d]',
      result: Ok(
        ListPattern(loc('1-12'), [
          BindingPattern(loc('2'), ident('a')),
          BindingPattern(loc('5'), ident('b')),
          BindingPattern(loc('8'), ident('c')),
          BindingPattern(loc('11'), ident('d')),
        ])
      ),
    },
  ].forEach(({source, result}) => {
    test(source, () => {
      expect(parsePattern(lex(source))).toEqual(result);
    });
  });
});

describe('comma list', () => {
  [
    {source: '()', result: Ok([])},
    {source: '(foo)', result: Ok(['foo'])},
    {source: '(foo, bar)', result: Ok(['foo', 'bar'])},
    {
      source: '(foo, bar, qux, lit)',
      result: Ok(['foo', 'bar', 'qux', 'lit']),
    },
    {
      source: '(,)',
      result: Err(
        UnexpectedTokenError(
          GlyphToken(loc('2'), Glyph.Comma),
          ExpectedIdentifier
        )
      ),
    },
    {source: '(foo,)', result: Ok(['foo'])},
    {source: '(foo, bar,)', result: Ok(['foo', 'bar'])},
    {
      source: '(foo, bar, qux, lit,)',
      result: Ok(['foo', 'bar', 'qux', 'lit']),
    },
    {
      source: '(%)',
      result: Err(
        UnexpectedTokenError(
          GlyphToken(loc('2'), Glyph.Percent),
          ExpectedIdentifier
        )
      ),
    },
    {
      source: '( % )',
      result: Err(
        UnexpectedTokenError(
          GlyphToken(loc('3'), Glyph.Percent),
          ExpectedIdentifier
        )
      ),
    },
    {
      source: '(foo bar)',
      result: Err(
        UnexpectedTokenError(
          IdentifierToken(loc('6-8'), ident('bar')),
          ExpectedGlyph(Glyph.Comma)
        )
      ),
    },
    {
      source: '(foo, bar qux)',
      result: Err(
        UnexpectedTokenError(
          IdentifierToken(loc('11-13'), ident('qux')),
          ExpectedGlyph(Glyph.Comma)
        )
      ),
    },
    {
      source: '(foo bar qux)',
      result: Err(
        UnexpectedTokenError(
          IdentifierToken(loc('6-8'), ident('bar')),
          ExpectedGlyph(Glyph.Comma)
        )
      ),
    },
    {
      source: '(foo,, bar)',
      result: Err(
        UnexpectedTokenError(
          GlyphToken(loc('6'), Glyph.Comma),
          ExpectedIdentifier
        )
      ),
    },
    {
      source: '(foo; bar)',
      result: Err(
        UnexpectedTokenError(
          GlyphToken(loc('5'), Glyph.Semicolon),
          ExpectedGlyph(Glyph.Comma)
        )
      ),
    },
    {
      source: '(foo;; bar)',
      result: Err(
        UnexpectedTokenError(
          GlyphToken(loc('5'), Glyph.Semicolon),
          ExpectedGlyph(Glyph.Comma)
        )
      ),
    },
    {
      source: '(foo, bar;)',
      result: Err(
        UnexpectedTokenError(
          GlyphToken(loc('10'), Glyph.Semicolon),
          ExpectedGlyph(Glyph.Comma)
        )
      ),
    },
    {
      source: '(',
      result: Err(UnexpectedTokenError(EndToken(loc('2')), ExpectedIdentifier)),
    },
    {
      source: '(foo',
      result: Err(
        UnexpectedTokenError(EndToken(loc('5')), ExpectedGlyph(Glyph.Comma))
      ),
    },
  ].forEach(({source, result}) => {
    test(source, () => {
      expect(parseCommaListTest(lex(source))).toEqual(result);
    });
  });
});
