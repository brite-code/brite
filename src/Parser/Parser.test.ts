import {Err, Ok} from '../Utils/Result';

import {
  BindingPattern,
  FunctionType,
  GenericType,
  HolePattern,
  MemberType,
  Name,
  QuantifiedType,
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
  ExpectedGlyph,
  ExpectedIdentifier,
  ExpectedPattern,
  ExpectedType,
  UnexpectedTokenError,
} from './Error';
import {Identifier, ident} from './Identifier';
import {EndToken, Glyph, GlyphToken, IdentifierToken, Lexer} from './Lexer';
import {loc} from './Loc';
import {parseCommaListTest, parsePattern, parseType} from './Parser';

function lex(source: string): Lexer {
  return Lexer.create(source);
}

describe('type', () => {
  test('empty string', () => {
    expect(parseType(lex(''))).toEqual(
      Err(UnexpectedTokenError(EndToken(loc('1')), ExpectedType))
    );
  });

  test('invalid string', () => {
    expect(parseType(lex('...'))).toEqual(
      Err(
        UnexpectedTokenError(
          GlyphToken(loc('1-3'), Glyph.Ellipsis),
          ExpectedType
        )
      )
    );
  });

  test('binding keyword', () => {
    expect(parseType(lex('if'))).toEqual(
      Err(
        UnexpectedTokenError(
          IdentifierToken(loc('1-2'), 'if' as Identifier),
          ExpectedType
        )
      )
    );
  });

  test('reference', () => {
    expect(parseType(lex('foo'))).toEqual(
      Ok(ReferenceType(loc('1-3'), ident('foo')))
    );
  });

  test('unit', () => {
    expect(parseType(lex('()'))).toEqual(Ok(UnitType(loc('1-2'))));
    expect(parseType(lex('( )'))).toEqual(Ok(UnitType(loc('1-3'))));
  });

  test('unit trailing comma', () => {
    expect(parseType(lex('(,)'))).toEqual(
      Err(UnexpectedTokenError(GlyphToken(loc('2'), Glyph.Comma), ExpectedType))
    );
  });

  test('wrapped', () => {
    expect(parseType(lex('(foo)'))).toEqual(
      Ok(WrappedType(loc('1-5'), ReferenceType(loc('2-4'), ident('foo'))))
    );
  });

  test('wrapped with trailing comma', () => {
    expect(parseType(lex('(foo,)'))).toEqual(
      Ok(WrappedType(loc('1-6'), ReferenceType(loc('2-4'), ident('foo'))))
    );
  });

  test('tuple with 2 elements', () => {
    expect(parseType(lex('(foo, bar)'))).toEqual(
      Ok(
        TupleType(loc('1-10'), [
          ReferenceType(loc('2-4'), ident('foo')),
          ReferenceType(loc('7-9'), ident('bar')),
        ])
      )
    );
  });

  test('tuple with 3 elements', () => {
    expect(parseType(lex('(foo, bar, qux)'))).toEqual(
      Ok(
        TupleType(loc('1-15'), [
          ReferenceType(loc('2-4'), ident('foo')),
          ReferenceType(loc('7-9'), ident('bar')),
          ReferenceType(loc('12-14'), ident('qux')),
        ])
      )
    );
  });

  test('tuple with 4 elements', () => {
    expect(parseType(lex('(foo, bar, qux, lit)'))).toEqual(
      Ok(
        TupleType(loc('1-20'), [
          ReferenceType(loc('2-4'), ident('foo')),
          ReferenceType(loc('7-9'), ident('bar')),
          ReferenceType(loc('12-14'), ident('qux')),
          ReferenceType(loc('17-19'), ident('lit')),
        ])
      )
    );
  });

  test('record with 0 properties', () => {
    expect(parseType(lex('{}'))).toEqual(Ok(RecordType(loc('1-2'), [])));
  });

  test('record with 1 property', () => {
    expect(parseType(lex('{ foo: T }'))).toEqual(
      Ok(
        RecordType(loc('1-10'), [
          RecordTypeProperty(
            Name(loc('3-5'), ident('foo')),
            ReferenceType(loc('8'), ident('T'))
          ),
        ])
      )
    );
  });

  test('record with 2 properties', () => {
    expect(parseType(lex('{ foo: T, bar: U }'))).toEqual(
      Ok(
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
      )
    );
  });

  test('record with 4 properties', () => {
    expect(parseType(lex('{ foo: T, bar: U, qux: V, lit: W }'))).toEqual(
      Ok(
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
      )
    );
  });

  test('record with optional property', () => {
    expect(parseType(lex('{ foo?: T }'))).toEqual(
      Ok(
        RecordType(loc('1-11'), [
          RecordTypeProperty.optional(
            Name(loc('3-5'), ident('foo')),
            ReferenceType(loc('9'), ident('T'))
          ),
        ])
      )
    );
  });

  test('member', () => {
    expect(parseType(lex('hello.world'))).toEqual(
      Ok(
        MemberType(
          loc('1-11'),
          ReferenceType(loc('1-5'), ident('hello')),
          Name(loc('7-11'), ident('world'))
        )
      )
    );
  });

  test('member twice', () => {
    expect(parseType(lex('foo.bar.qux'))).toEqual(
      Ok(
        MemberType(
          loc('1-11'),
          MemberType(
            loc('1-7'),
            ReferenceType(loc('1-3'), ident('foo')),
            Name(loc('5-7'), ident('bar'))
          ),
          Name(loc('9-11'), ident('qux'))
        )
      )
    );
  });

  test('member thrice', () => {
    expect(parseType(lex('foo.bar.qux.lit'))).toEqual(
      Ok(
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
      )
    );
  });

  test('member without identifier', () => {
    expect(parseType(lex('hello.'))).toEqual(
      Err(UnexpectedTokenError(EndToken(loc('7')), ExpectedIdentifier))
    );
    expect(parseType(lex('hello.%'))).toEqual(
      Err(
        UnexpectedTokenError(
          GlyphToken(loc('7'), Glyph.Percent),
          ExpectedIdentifier
        )
      )
    );
  });

  test('generic with 0 arguments', () => {
    expect(parseType(lex('T<>'))).toEqual(
      Ok(GenericType(loc('1-3'), ReferenceType(loc('1'), ident('T')), []))
    );
  });

  test('generic with 1 argument', () => {
    expect(parseType(lex('T<A>'))).toEqual(
      Ok(
        GenericType(loc('1-4'), ReferenceType(loc('1'), ident('T')), [
          ReferenceType(loc('3'), ident('A')),
        ])
      )
    );
  });

  test('generic with 2 arguments', () => {
    expect(parseType(lex('T<A, B>'))).toEqual(
      Ok(
        GenericType(loc('1-7'), ReferenceType(loc('1'), ident('T')), [
          ReferenceType(loc('3'), ident('A')),
          ReferenceType(loc('6'), ident('B')),
        ])
      )
    );
  });

  test('generic with 4 arguments', () => {
    expect(parseType(lex('T<A, B, C, D>'))).toEqual(
      Ok(
        GenericType(loc('1-13'), ReferenceType(loc('1'), ident('T')), [
          ReferenceType(loc('3'), ident('A')),
          ReferenceType(loc('6'), ident('B')),
          ReferenceType(loc('9'), ident('C')),
          ReferenceType(loc('12'), ident('D')),
        ])
      )
    );
  });

  test('generic after member', () => {
    expect(parseType(lex('React.Component<Props>'))).toEqual(
      Ok(
        GenericType(
          loc('1-22'),
          MemberType(
            loc('1-15'),
            ReferenceType(loc('1-5'), ident('React')),
            Name(loc('7-15'), ident('Component'))
          ),
          [ReferenceType(loc('17-21'), ident('Props'))]
        )
      )
    );
  });

  test('member after generic', () => {
    expect(parseType(lex('A<>.B'))).toEqual(
      Ok(
        MemberType(
          loc('1-5'),
          GenericType(loc('1-3'), ReferenceType(loc('1'), ident('A')), []),
          Name(loc('5'), ident('B'))
        )
      )
    );
  });

  test('generic after generic', () => {
    expect(parseType(lex('A<><>'))).toEqual(
      Ok(
        GenericType(
          loc('1-5'),
          GenericType(loc('1-3'), ReferenceType(loc('1'), ident('A')), []),
          []
        )
      )
    );
  });

  test('unit member', () => {
    expect(parseType(lex('().foo'))).toEqual(
      Ok(
        MemberType(
          loc('1-6'),
          UnitType(loc('1-2')),
          Name(loc('4-6'), ident('foo'))
        )
      )
    );
  });

  test('record member', () => {
    expect(parseType(lex('{}.foo'))).toEqual(
      Ok(
        MemberType(
          loc('1-6'),
          RecordType(loc('1-2'), []),
          Name(loc('4-6'), ident('foo'))
        )
      )
    );
  });

  test('function with 0 parameters', () => {
    expect(parseType(lex('() -> ()'))).toEqual(
      Ok(FunctionType(loc('1-8'), [], UnitType(loc('7-8'))))
    );
  });

  test('function with 1 parameter', () => {
    expect(parseType(lex('(a) -> ()'))).toEqual(
      Ok(
        FunctionType(
          loc('1-9'),
          [ReferenceType(loc('2'), ident('a'))],
          UnitType(loc('8-9'))
        )
      )
    );
  });

  test('function with 2 parameters', () => {
    expect(parseType(lex('(a, b) -> ()'))).toEqual(
      Ok(
        FunctionType(
          loc('1-12'),
          [
            ReferenceType(loc('2'), ident('a')),
            ReferenceType(loc('5'), ident('b')),
          ],
          UnitType(loc('11-12'))
        )
      )
    );
  });

  test('function with 4 parameters', () => {
    expect(parseType(lex('(a, b, c, d) -> ()'))).toEqual(
      Ok(
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
      )
    );
  });

  test('function returning member', () => {
    expect(parseType(lex('() -> foo.bar'))).toEqual(
      Ok(
        FunctionType(
          loc('1-13'),
          [],
          MemberType(
            loc('7-13'),
            ReferenceType(loc('7-9'), ident('foo')),
            Name(loc('11-13'), ident('bar'))
          )
        )
      )
    );
  });

  test('function no body', () => {
    expect(parseType(lex('() ->'))).toEqual(
      Err(UnexpectedTokenError(EndToken(loc('6')), ExpectedType))
    );
  });

  test('function identifier shorthand', () => {
    expect(parseType(lex('T -> T'))).toEqual(
      Ok(
        FunctionType(
          loc('1-6'),
          [ReferenceType(loc('1'), ident('T'))],
          ReferenceType(loc('6'), ident('T'))
        )
      )
    );
  });

  test('function identifier shorthand returning member', () => {
    expect(parseType(lex('x -> foo.bar'))).toEqual(
      Ok(
        FunctionType(
          loc('1-12'),
          [ReferenceType(loc('1'), ident('x'))],
          MemberType(
            loc('6-12'),
            ReferenceType(loc('6-8'), ident('foo')),
            Name(loc('10-12'), ident('bar'))
          )
        )
      )
    );
  });

  test('tuple skips errors', () => {
    expect(parseType(lex('(X, Y, %, Z)'))).toEqual(
      Err(
        UnexpectedTokenError(GlyphToken(loc('8'), Glyph.Percent), ExpectedType)
      )
    );
  });

  test('generic skips errors', () => {
    expect(parseType(lex('T<X, Y, %, Z>'))).toEqual(
      Err(
        UnexpectedTokenError(GlyphToken(loc('9'), Glyph.Percent), ExpectedType)
      )
    );
  });

  test('quantified', () => {
    expect(parseType(lex('<T> T'))).toEqual(
      Ok(
        QuantifiedType(
          loc('1-5'),
          [TypeParameter(Name(loc('2'), ident('T')), [])],
          ReferenceType(loc('5'), ident('T'))
        )
      )
    );
  });

  test('quantified generic', () => {
    expect(parseType(lex('<T> Foo<T>'))).toEqual(
      Ok(
        QuantifiedType(
          loc('1-10'),
          [TypeParameter(Name(loc('2'), ident('T')), [])],
          GenericType(loc('5-10'), ReferenceType(loc('5-7'), ident('Foo')), [
            ReferenceType(loc('9'), ident('T')),
          ])
        )
      )
    );
  });

  test('quantified function', () => {
    expect(parseType(lex('<T>(T) -> T'))).toEqual(
      Ok(
        QuantifiedType(
          loc('1-11'),
          [TypeParameter(Name(loc('2'), ident('T')), [])],
          FunctionType(
            loc('4-11'),
            [ReferenceType(loc('5'), ident('T'))],
            ReferenceType(loc('11'), ident('T'))
          )
        )
      )
    );
  });

  test('quantified function with identifier shorthand', () => {
    expect(parseType(lex('<T> T -> T'))).toEqual(
      Ok(
        QuantifiedType(
          loc('1-10'),
          [TypeParameter(Name(loc('2'), ident('T')), [])],
          FunctionType(
            loc('5-10'),
            [ReferenceType(loc('5'), ident('T'))],
            ReferenceType(loc('10'), ident('T'))
          )
        )
      )
    );
  });

  test('quantified error body', () => {
    expect(parseType(lex('<T> %'))).toEqual(
      Err(
        UnexpectedTokenError(GlyphToken(loc('5'), Glyph.Percent), ExpectedType)
      )
    );
  });

  test('quantified with 1 bound', () => {
    expect(parseType(lex('<T: A> T'))).toEqual(
      Ok(
        QuantifiedType(
          loc('1-8'),
          [
            TypeParameter(Name(loc('2'), ident('T')), [
              ReferenceType(loc('5'), ident('A')),
            ]),
          ],
          ReferenceType(loc('8'), ident('T'))
        )
      )
    );
  });

  test('quantified with 2 bounds', () => {
    expect(parseType(lex('<T: A + B> T'))).toEqual(
      Ok(
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
      )
    );
  });

  test('quantified with 4 bounds', () => {
    expect(parseType(lex('<T: A + B + C + D> T'))).toEqual(
      Ok(
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
      )
    );
  });

  test('quantified with 0 bounds', () => {
    expect(parseType(lex('<T: > T'))).toEqual(
      Err(
        UnexpectedTokenError(
          GlyphToken(loc('5'), Glyph.GreaterThan),
          ExpectedType
        )
      )
    );
  });

  test('quantified with 1 incorrect bound', () => {
    expect(parseType(lex('<T: %> T'))).toEqual(
      Err(
        UnexpectedTokenError(GlyphToken(loc('5'), Glyph.Percent), ExpectedType)
      )
    );
  });

  test('quantified with 1 correct and 1 incorrect bound', () => {
    expect(parseType(lex('<T: A + %> T'))).toEqual(
      Err(
        UnexpectedTokenError(GlyphToken(loc('9'), Glyph.Percent), ExpectedType)
      )
    );
  });

  test('quantified with no plus for bounds', () => {
    expect(parseType(lex('<T: A B> T'))).toEqual(
      Err(
        UnexpectedTokenError(
          IdentifierToken(loc('7'), ident('B')),
          ExpectedGlyph(Glyph.Comma)
        )
      )
    );
  });
});

describe('pattern', () => {
  test('binding', () => {
    expect(parsePattern(lex('foo'))).toEqual(
      Ok(BindingPattern(loc('1-3'), ident('foo')))
    );
  });

  test('binding keyword', () => {
    expect(parsePattern(lex('if'))).toEqual(
      Err(
        UnexpectedTokenError(
          IdentifierToken(loc('1-2'), 'if' as Identifier),
          ExpectedPattern
        )
      )
    );
  });

  test('hole', () => {
    expect(parsePattern(lex('_'))).toEqual(Ok(HolePattern(loc('1'))));
  });

  test('binding underscore prefixed', () => {
    expect(parsePattern(lex('_x'))).toEqual(
      Ok(BindingPattern(loc('1-2'), ident('_x')))
    );
  });

  test('unit', () => {
    expect(parsePattern(lex('()'))).toEqual(Ok(UnitPattern(loc('1-2'))));
  });

  test('wrapped', () => {
    expect(parsePattern(lex('(foo)'))).toEqual(
      Ok(
        WrappedPattern(
          loc('1-5'),
          BindingPattern(loc('2-4'), ident('foo')),
          undefined
        )
      )
    );
  });

  test('tuple with 2 elements', () => {
    expect(parsePattern(lex('(a, b)'))).toEqual(
      Ok(
        TuplePattern(loc('1-6'), [
          TuplePatternElement(BindingPattern(loc('2'), ident('a')), undefined),
          TuplePatternElement(BindingPattern(loc('5'), ident('b')), undefined),
        ])
      )
    );
  });

  test('tuple with 4 elements', () => {
    expect(parsePattern(lex('(a, b, c, d)'))).toEqual(
      Ok(
        TuplePattern(loc('1-12'), [
          TuplePatternElement(BindingPattern(loc('2'), ident('a')), undefined),
          TuplePatternElement(BindingPattern(loc('5'), ident('b')), undefined),
          TuplePatternElement(BindingPattern(loc('8'), ident('c')), undefined),
          TuplePatternElement(BindingPattern(loc('11'), ident('d')), undefined),
        ])
      )
    );
  });

  test('unit with trailing comma', () => {
    expect(parsePattern(lex('(,)'))).toEqual(
      Err(
        UnexpectedTokenError(GlyphToken(loc('2'), Glyph.Comma), ExpectedPattern)
      )
    );
  });

  test('wrapped with trailing comma', () => {
    expect(parsePattern(lex('(foo,)'))).toEqual(
      Ok(
        WrappedPattern(
          loc('1-6'),
          BindingPattern(loc('2-4'), ident('foo')),
          undefined
        )
      )
    );
  });

  test('tuple with 2 elements and trailing comma', () => {
    expect(parsePattern(lex('(a, b,)'))).toEqual(
      Ok(
        TuplePattern(loc('1-7'), [
          TuplePatternElement(BindingPattern(loc('2'), ident('a')), undefined),
          TuplePatternElement(BindingPattern(loc('5'), ident('b')), undefined),
        ])
      )
    );
  });

  test('tuple with 4 elements and trailing comma', () => {
    expect(parsePattern(lex('(a, b, c, d,)'))).toEqual(
      Ok(
        TuplePattern(loc('1-13'), [
          TuplePatternElement(BindingPattern(loc('2'), ident('a')), undefined),
          TuplePatternElement(BindingPattern(loc('5'), ident('b')), undefined),
          TuplePatternElement(BindingPattern(loc('8'), ident('c')), undefined),
          TuplePatternElement(BindingPattern(loc('11'), ident('d')), undefined),
        ])
      )
    );
  });

  test('wrapped expecting annotation', () => {
    expect(parsePattern(lex('(x:)'))).toEqual(
      Err(
        UnexpectedTokenError(
          GlyphToken(loc('4'), Glyph.ParenRight),
          ExpectedType
        )
      )
    );
  });

  test('wrapped with annotation', () => {
    expect(parsePattern(lex('(x: T)'))).toEqual(
      Ok(
        WrappedPattern(
          loc('1-6'),
          BindingPattern(loc('2'), ident('x')),
          ReferenceType(loc('5'), ident('T'))
        )
      )
    );
  });

  test('tuple with 2 elements and annotations', () => {
    expect(parsePattern(lex('(a: A, b: B)'))).toEqual(
      Ok(
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
      )
    );
  });

  test('tuple with 4 elements and annotations', () => {
    expect(parsePattern(lex('(a: A, b: B, c: C, d: D)'))).toEqual(
      Ok(
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
      )
    );
  });

  test('wrapped with annotation and trailing comma', () => {
    expect(parsePattern(lex('(x: T,)'))).toEqual(
      Ok(
        WrappedPattern(
          loc('1-7'),
          BindingPattern(loc('2'), ident('x')),
          ReferenceType(loc('5'), ident('T'))
        )
      )
    );
  });

  test('tuple with 2 elements and annotations and trailing comma', () => {
    expect(parsePattern(lex('(a: A, b: B,)'))).toEqual(
      Ok(
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
      )
    );
  });
});

describe('comma list', () => {
  test('zero', () => {
    expect(parseCommaListTest(lex('()'))).toEqual(Ok([]));
  });

  test('one', () => {
    expect(parseCommaListTest(lex('(foo)'))).toEqual(Ok(['foo']));
  });

  test('two', () => {
    expect(parseCommaListTest(lex('(foo, bar)'))).toEqual(Ok(['foo', 'bar']));
  });

  test('four', () => {
    expect(parseCommaListTest(lex('(foo, bar, qux, lit)'))).toEqual(
      Ok(['foo', 'bar', 'qux', 'lit'])
    );
  });

  test('zero trailing comma', () => {
    expect(parseCommaListTest(lex('(,)'))).toEqual(
      Err(
        UnexpectedTokenError(
          GlyphToken(loc('2'), Glyph.Comma),
          ExpectedIdentifier
        )
      )
    );
  });

  test('one trailing comma', () => {
    expect(parseCommaListTest(lex('(foo,)'))).toEqual(Ok(['foo']));
  });

  test('two trailing comma', () => {
    expect(parseCommaListTest(lex('(foo, bar,)'))).toEqual(Ok(['foo', 'bar']));
  });

  test('four trailing comma', () => {
    expect(parseCommaListTest(lex('(foo, bar, qux, lit,)'))).toEqual(
      Ok(['foo', 'bar', 'qux', 'lit'])
    );
  });

  test('random character', () => {
    expect(parseCommaListTest(lex('(%)'))).toEqual(
      Err(
        UnexpectedTokenError(
          GlyphToken(loc('2'), Glyph.Percent),
          ExpectedIdentifier
        )
      )
    );
    expect(parseCommaListTest(lex('( % )'))).toEqual(
      Err(
        UnexpectedTokenError(
          GlyphToken(loc('3'), Glyph.Percent),
          ExpectedIdentifier
        )
      )
    );
  });

  test('no commas', () => {
    expect(parseCommaListTest(lex('(foo bar)'))).toEqual(
      Err(
        UnexpectedTokenError(
          IdentifierToken(loc('6-8'), ident('bar')),
          ExpectedGlyph(Glyph.Comma)
        )
      )
    );
  });

  test('missing one comma of two', () => {
    expect(parseCommaListTest(lex('(foo, bar qux)'))).toEqual(
      Err(
        UnexpectedTokenError(
          IdentifierToken(loc('11-13'), ident('qux')),
          ExpectedGlyph(Glyph.Comma)
        )
      )
    );
  });

  test('missing two commas of two', () => {
    expect(parseCommaListTest(lex('(foo bar qux)'))).toEqual(
      Err(
        UnexpectedTokenError(
          IdentifierToken(loc('6-8'), ident('bar')),
          ExpectedGlyph(Glyph.Comma)
        )
      )
    );
  });

  test('double comma', () => {
    expect(parseCommaListTest(lex('(foo,, bar)'))).toEqual(
      Err(
        UnexpectedTokenError(
          GlyphToken(loc('6'), Glyph.Comma),
          ExpectedIdentifier
        )
      )
    );
  });

  test('non-comma separator', () => {
    expect(parseCommaListTest(lex('(foo; bar)'))).toEqual(
      Err(
        UnexpectedTokenError(
          GlyphToken(loc('5'), Glyph.Semicolon),
          ExpectedGlyph(Glyph.Comma)
        )
      )
    );
  });

  test('double non-comma separator', () => {
    expect(parseCommaListTest(lex('(foo;; bar)'))).toEqual(
      Err(
        UnexpectedTokenError(
          GlyphToken(loc('5'), Glyph.Semicolon),
          ExpectedGlyph(Glyph.Comma)
        )
      )
    );
  });

  test('trailing non-comma separator', () => {
    expect(parseCommaListTest(lex('(foo, bar;)'))).toEqual(
      Err(
        UnexpectedTokenError(
          GlyphToken(loc('10'), Glyph.Semicolon),
          ExpectedGlyph(Glyph.Comma)
        )
      )
    );
  });

  test('not ended when expecting item', () => {
    expect(parseCommaListTest(lex('('))).toEqual(
      Err(UnexpectedTokenError(EndToken(loc('2')), ExpectedIdentifier))
    );
  });

  test('not ended when expecting comma', () => {
    expect(parseCommaListTest(lex('(foo'))).toEqual(
      Err(UnexpectedTokenError(EndToken(loc('5')), ExpectedGlyph(Glyph.Comma)))
    );
  });
});
