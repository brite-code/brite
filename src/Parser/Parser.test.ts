import {
  ErrorType,
  FunctionType,
  GenericType,
  MemberType,
  Name,
  QuantifiedType,
  RecordType,
  RecordTypeProperty,
  ReferenceType,
  TupleType,
  TypeParameter,
  UnitType,
  WrappedType,
} from './Ast';
import {
  ExpectedEnd,
  ExpectedGlyph,
  ExpectedIdentifier,
  ExpectedType,
  UnexpectedTokenError,
} from './Error';
import {ident} from './Identifier';
import {Glyph, Lexer, TokenType} from './Lexer';
import {loc} from './Loc';
import {parseCommaListTest, parseType} from './Parser';

function lex(source: string): Lexer {
  return Lexer.create(source);
}

describe('type', () => {
  test('empty string', () => {
    const error = UnexpectedTokenError(
      {type: TokenType.End, loc: loc('1-1')},
      ExpectedType
    );
    expect(parseType(lex(''))).toEqual({
      errors: [error],
      type: ErrorType(loc('1-1'), error),
    });
  });

  test('invalid string', () => {
    const error = UnexpectedTokenError(
      {type: TokenType.Glyph, loc: loc('1-3'), glyph: Glyph.Ellipsis},
      ExpectedType
    );
    expect(parseType(lex('...'))).toEqual({
      errors: [error],
      type: ErrorType(loc('1-3'), error),
    });
  });

  test('binding keyword', () => {
    const error = UnexpectedTokenError(
      {
        type: TokenType.Identifier,
        loc: loc('1-2'),
        identifier: ident('if'),
      },
      ExpectedType
    );
    expect(parseType(lex('if'))).toEqual({
      errors: [error],
      type: ErrorType(loc('1-2'), error),
    });
  });

  test('reference', () => {
    expect(parseType(lex('foo'))).toEqual({
      errors: [],
      type: ReferenceType(loc('1-3'), ident('foo')),
    });
  });

  test('unit', () => {
    expect(parseType(lex('()'))).toEqual({
      errors: [],
      type: UnitType(loc('1-2')),
    });
    expect(parseType(lex('( )'))).toEqual({
      errors: [],
      type: UnitType(loc('1-3')),
    });
  });

  test('unit trailing comma', () => {
    expect(parseType(lex('(,)'))).toEqual({
      errors: [
        UnexpectedTokenError(
          {type: TokenType.Glyph, loc: loc('2'), glyph: Glyph.Comma},
          ExpectedType
        ),
      ],
      type: UnitType(loc('1-3')),
    });
  });

  test('wrapped', () => {
    expect(parseType(lex('(foo)'))).toEqual({
      errors: [],
      type: WrappedType(loc('1-5'), ReferenceType(loc('2-4'), ident('foo'))),
    });
  });

  test('wrapped with trailing comma', () => {
    expect(parseType(lex('(foo,)'))).toEqual({
      errors: [],
      type: WrappedType(loc('1-6'), ReferenceType(loc('2-4'), ident('foo'))),
    });
  });

  test('tuple with 2 elements', () => {
    expect(parseType(lex('(foo, bar)'))).toEqual({
      errors: [],
      type: TupleType(loc('1-10'), [
        ReferenceType(loc('2-4'), ident('foo')),
        ReferenceType(loc('7-9'), ident('bar')),
      ]),
    });
  });

  test('tuple with 3 elements', () => {
    expect(parseType(lex('(foo, bar, qux)'))).toEqual({
      errors: [],
      type: TupleType(loc('1-15'), [
        ReferenceType(loc('2-4'), ident('foo')),
        ReferenceType(loc('7-9'), ident('bar')),
        ReferenceType(loc('12-14'), ident('qux')),
      ]),
    });
  });

  test('tuple with 4 elements', () => {
    expect(parseType(lex('(foo, bar, qux, lit)'))).toEqual({
      errors: [],
      type: TupleType(loc('1-20'), [
        ReferenceType(loc('2-4'), ident('foo')),
        ReferenceType(loc('7-9'), ident('bar')),
        ReferenceType(loc('12-14'), ident('qux')),
        ReferenceType(loc('17-19'), ident('lit')),
      ]),
    });
  });

  test('record with 0 properties', () => {
    expect(parseType(lex('{}'))).toEqual({
      errors: [],
      type: RecordType(loc('1-2'), []),
    });
  });

  test('record with 1 property', () => {
    expect(parseType(lex('{ foo: T }'))).toEqual({
      errors: [],
      type: RecordType(loc('1-10'), [
        RecordTypeProperty(
          Name(loc('3-5'), ident('foo')),
          ReferenceType(loc('8'), ident('T'))
        ),
      ]),
    });
  });

  test('record with 2 properties', () => {
    expect(parseType(lex('{ foo: T, bar: U }'))).toEqual({
      errors: [],
      type: RecordType(loc('1-18'), [
        RecordTypeProperty(
          Name(loc('3-5'), ident('foo')),
          ReferenceType(loc('8'), ident('T'))
        ),
        RecordTypeProperty(
          Name(loc('11-13'), ident('bar')),
          ReferenceType(loc('16'), ident('U'))
        ),
      ]),
    });
  });

  test('record with 4 properties', () => {
    expect(parseType(lex('{ foo: T, bar: U, qux: V, lit: W }'))).toEqual({
      errors: [],
      type: RecordType(loc('1-34'), [
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
      ]),
    });
  });

  test('record with optional property', () => {
    expect(parseType(lex('{ foo?: T }'))).toEqual({
      errors: [],
      type: RecordType(loc('1-11'), [
        RecordTypeProperty.optional(
          Name(loc('3-5'), ident('foo')),
          ReferenceType(loc('9'), ident('T'))
        ),
      ]),
    });
  });

  test('member', () => {
    expect(parseType(lex('hello.world'))).toEqual({
      errors: [],
      type: MemberType(
        loc('1-11'),
        ReferenceType(loc('1-5'), ident('hello')),
        Name(loc('7-11'), ident('world'))
      ),
    });
  });

  test('member twice', () => {
    expect(parseType(lex('foo.bar.qux'))).toEqual({
      errors: [],
      type: MemberType(
        loc('1-11'),
        MemberType(
          loc('1-7'),
          ReferenceType(loc('1-3'), ident('foo')),
          Name(loc('5-7'), ident('bar'))
        ),
        Name(loc('9-11'), ident('qux'))
      ),
    });
  });

  test('member thrice', () => {
    expect(parseType(lex('foo.bar.qux.lit'))).toEqual({
      errors: [],
      type: MemberType(
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
      ),
    });
  });

  test('member without identifier', () => {
    expect(parseType(lex('hello.'))).toEqual({
      errors: [
        UnexpectedTokenError(
          {type: TokenType.End, loc: loc('7')},
          ExpectedIdentifier
        ),
      ],
      type: ReferenceType(loc('1-5'), ident('hello')),
    });
    expect(parseType(lex('hello.%'))).toEqual({
      errors: [
        UnexpectedTokenError(
          {type: TokenType.Glyph, loc: loc('7'), glyph: Glyph.Percent},
          ExpectedIdentifier
        ),
        UnexpectedTokenError(
          {type: TokenType.Glyph, loc: loc('7'), glyph: Glyph.Percent},
          ExpectedEnd
        ),
      ],
      type: ReferenceType(loc('1-5'), ident('hello')),
    });
  });

  test('generic with 0 arguments', () => {
    expect(parseType(lex('T<>'))).toEqual({
      errors: [],
      type: GenericType(loc('1-3'), ReferenceType(loc('1'), ident('T')), []),
    });
  });

  test('generic with 1 argument', () => {
    expect(parseType(lex('T<A>'))).toEqual({
      errors: [],
      type: GenericType(loc('1-4'), ReferenceType(loc('1'), ident('T')), [
        ReferenceType(loc('3'), ident('A')),
      ]),
    });
  });

  test('generic with 2 arguments', () => {
    expect(parseType(lex('T<A, B>'))).toEqual({
      errors: [],
      type: GenericType(loc('1-7'), ReferenceType(loc('1'), ident('T')), [
        ReferenceType(loc('3'), ident('A')),
        ReferenceType(loc('6'), ident('B')),
      ]),
    });
  });

  test('generic with 4 arguments', () => {
    expect(parseType(lex('T<A, B, C, D>'))).toEqual({
      errors: [],
      type: GenericType(loc('1-13'), ReferenceType(loc('1'), ident('T')), [
        ReferenceType(loc('3'), ident('A')),
        ReferenceType(loc('6'), ident('B')),
        ReferenceType(loc('9'), ident('C')),
        ReferenceType(loc('12'), ident('D')),
      ]),
    });
  });

  test('generic after member', () => {
    expect(parseType(lex('React.Component<Props>'))).toEqual({
      errors: [],
      type: GenericType(
        loc('1-22'),
        MemberType(
          loc('1-15'),
          ReferenceType(loc('1-5'), ident('React')),
          Name(loc('7-15'), ident('Component'))
        ),
        [ReferenceType(loc('17-21'), ident('Props'))]
      ),
    });
  });

  test('member after generic', () => {
    expect(parseType(lex('A<>.B'))).toEqual({
      errors: [],
      type: MemberType(
        loc('1-5'),
        GenericType(loc('1-3'), ReferenceType(loc('1'), ident('A')), []),
        Name(loc('5'), ident('B'))
      ),
    });
  });

  test('generic after generic', () => {
    expect(parseType(lex('A<><>'))).toEqual({
      errors: [],
      type: GenericType(
        loc('1-5'),
        GenericType(loc('1-3'), ReferenceType(loc('1'), ident('A')), []),
        []
      ),
    });
  });

  test('unit member', () => {
    expect(parseType(lex('().foo'))).toEqual({
      errors: [],
      type: MemberType(
        loc('1-6'),
        UnitType(loc('1-2')),
        Name(loc('4-6'), ident('foo'))
      ),
    });
  });

  test('record member', () => {
    expect(parseType(lex('{}.foo'))).toEqual({
      errors: [],
      type: MemberType(
        loc('1-6'),
        RecordType(loc('1-2'), []),
        Name(loc('4-6'), ident('foo'))
      ),
    });
  });

  test('function with 0 parameters', () => {
    expect(parseType(lex('() -> ()'))).toEqual({
      errors: [],
      type: FunctionType(loc('1-8'), [], UnitType(loc('7-8'))),
    });
  });

  test('function with 1 parameter', () => {
    expect(parseType(lex('(a) -> ()'))).toEqual({
      errors: [],
      type: FunctionType(
        loc('1-9'),
        [ReferenceType(loc('2'), ident('a'))],
        UnitType(loc('8-9'))
      ),
    });
  });

  test('function with 2 parameters', () => {
    expect(parseType(lex('(a, b) -> ()'))).toEqual({
      errors: [],
      type: FunctionType(
        loc('1-12'),
        [
          ReferenceType(loc('2'), ident('a')),
          ReferenceType(loc('5'), ident('b')),
        ],
        UnitType(loc('11-12'))
      ),
    });
  });

  test('function with 4 parameters', () => {
    expect(parseType(lex('(a, b, c, d) -> ()'))).toEqual({
      errors: [],
      type: FunctionType(
        loc('1-18'),
        [
          ReferenceType(loc('2'), ident('a')),
          ReferenceType(loc('5'), ident('b')),
          ReferenceType(loc('8'), ident('c')),
          ReferenceType(loc('11'), ident('d')),
        ],
        UnitType(loc('17-18'))
      ),
    });
  });

  test('function returning member', () => {
    expect(parseType(lex('() -> foo.bar'))).toEqual({
      errors: [],
      type: FunctionType(
        loc('1-13'),
        [],
        MemberType(
          loc('7-13'),
          ReferenceType(loc('7-9'), ident('foo')),
          Name(loc('11-13'), ident('bar'))
        )
      ),
    });
  });

  test('function no body', () => {
    const error = UnexpectedTokenError(
      {type: TokenType.End, loc: loc('6')},
      ExpectedType
    );
    expect(parseType(lex('() ->'))).toEqual({
      errors: [error],
      type: FunctionType(loc('1-6'), [], ErrorType(loc('6'), error)),
    });
  });

  test('function identifier shorthand', () => {
    expect(parseType(lex('T -> T'))).toEqual({
      errors: [],
      type: FunctionType(
        loc('1-6'),
        [ReferenceType(loc('1'), ident('T'))],
        ReferenceType(loc('6'), ident('T'))
      ),
    });
  });

  test('function identifier shorthand returning member', () => {
    expect(parseType(lex('x -> foo.bar'))).toEqual({
      errors: [],
      type: FunctionType(
        loc('1-12'),
        [ReferenceType(loc('1'), ident('x'))],
        MemberType(
          loc('6-12'),
          ReferenceType(loc('6-8'), ident('foo')),
          Name(loc('10-12'), ident('bar'))
        )
      ),
    });
  });

  test('tuple skips errors', () => {
    expect(parseType(lex('(X, Y, %, Z)'))).toEqual({
      errors: [
        UnexpectedTokenError(
          {type: TokenType.Glyph, loc: loc('8'), glyph: Glyph.Percent},
          ExpectedType
        ),
        UnexpectedTokenError(
          {type: TokenType.Glyph, loc: loc('9'), glyph: Glyph.Comma},
          ExpectedType
        ),
      ],
      type: TupleType(loc('1-12'), [
        ReferenceType(loc('2'), ident('X')),
        ReferenceType(loc('5'), ident('Y')),
        ReferenceType(loc('11'), ident('Z')),
      ]),
    });
  });

  test('generic skips errors', () => {
    expect(parseType(lex('T<X, Y, %, Z>'))).toEqual({
      errors: [
        UnexpectedTokenError(
          {type: TokenType.Glyph, loc: loc('9'), glyph: Glyph.Percent},
          ExpectedType
        ),
        UnexpectedTokenError(
          {type: TokenType.Glyph, loc: loc('10'), glyph: Glyph.Comma},
          ExpectedType
        ),
      ],
      type: GenericType(loc('1-13'), ReferenceType(loc('1'), ident('T')), [
        ReferenceType(loc('3'), ident('X')),
        ReferenceType(loc('6'), ident('Y')),
        ReferenceType(loc('12'), ident('Z')),
      ]),
    });
  });

  test('quantified', () => {
    expect(parseType(lex('<T> T'))).toEqual({
      errors: [],
      type: QuantifiedType(
        loc('1-5'),
        [TypeParameter(Name(loc('2'), ident('T')), [])],
        ReferenceType(loc('5'), ident('T'))
      ),
    });
  });

  test('quantified generic', () => {
    expect(parseType(lex('<T> Foo<T>'))).toEqual({
      errors: [],
      type: QuantifiedType(
        loc('1-10'),
        [TypeParameter(Name(loc('2'), ident('T')), [])],
        GenericType(loc('5-10'), ReferenceType(loc('5-7'), ident('Foo')), [
          ReferenceType(loc('9'), ident('T')),
        ])
      ),
    });
  });

  test('quantified function', () => {
    expect(parseType(lex('<T>(T) -> T'))).toEqual({
      errors: [],
      type: QuantifiedType(
        loc('1-11'),
        [TypeParameter(Name(loc('2'), ident('T')), [])],
        FunctionType(
          loc('4-11'),
          [ReferenceType(loc('5'), ident('T'))],
          ReferenceType(loc('11'), ident('T'))
        )
      ),
    });
  });

  test('quantified function with identifier shorthand', () => {
    expect(parseType(lex('<T> T -> T'))).toEqual({
      errors: [],
      type: QuantifiedType(
        loc('1-10'),
        [TypeParameter(Name(loc('2'), ident('T')), [])],
        FunctionType(
          loc('5-10'),
          [ReferenceType(loc('5'), ident('T'))],
          ReferenceType(loc('10'), ident('T'))
        )
      ),
    });
  });

  test('quantified error body', () => {
    const error = UnexpectedTokenError(
      {type: TokenType.Glyph, loc: loc('5'), glyph: Glyph.Percent},
      ExpectedType
    );
    expect(parseType(lex('<T> %'))).toEqual({
      errors: [error],
      type: QuantifiedType(
        loc('1-5'),
        [TypeParameter(Name(loc('2'), ident('T')), [])],
        ErrorType(loc('5'), error)
      ),
    });
  });
});

describe('comma list', () => {
  test('zero', () => {
    expect(parseCommaListTest(lex('()'))).toEqual({
      errors: [],
      result: [],
    });
  });

  test('one', () => {
    expect(parseCommaListTest(lex('(foo)'))).toEqual({
      errors: [],
      result: ['foo'],
    });
  });

  test('two', () => {
    expect(parseCommaListTest(lex('(foo, bar)'))).toEqual({
      errors: [],
      result: ['foo', 'bar'],
    });
  });

  test('four', () => {
    expect(parseCommaListTest(lex('(foo, bar, qux, lit)'))).toEqual({
      errors: [],
      result: ['foo', 'bar', 'qux', 'lit'],
    });
  });

  test('zero trailing comma', () => {
    expect(parseCommaListTest(lex('(,)'))).toEqual({
      errors: [
        UnexpectedTokenError(
          {type: TokenType.Glyph, loc: loc('2'), glyph: Glyph.Comma},
          ExpectedIdentifier
        ),
      ],
      result: [],
    });
  });

  test('one trailing comma', () => {
    expect(parseCommaListTest(lex('(foo,)'))).toEqual({
      errors: [],
      result: ['foo'],
    });
  });

  test('two trailing comma', () => {
    expect(parseCommaListTest(lex('(foo, bar,)'))).toEqual({
      errors: [],
      result: ['foo', 'bar'],
    });
  });

  test('four trailing comma', () => {
    expect(parseCommaListTest(lex('(foo, bar, qux, lit,)'))).toEqual({
      errors: [],
      result: ['foo', 'bar', 'qux', 'lit'],
    });
  });

  test('random character', () => {
    expect(parseCommaListTest(lex('(%)'))).toEqual({
      errors: [
        UnexpectedTokenError(
          {type: TokenType.Glyph, loc: loc('2'), glyph: Glyph.Percent},
          ExpectedIdentifier
        ),
      ],
      result: [],
    });
    expect(parseCommaListTest(lex('( % )'))).toEqual({
      errors: [
        UnexpectedTokenError(
          {type: TokenType.Glyph, loc: loc('3'), glyph: Glyph.Percent},
          ExpectedIdentifier
        ),
      ],
      result: [],
    });
  });

  test('no commas', () => {
    expect(parseCommaListTest(lex('(foo bar)'))).toEqual({
      errors: [
        UnexpectedTokenError(
          {
            type: TokenType.Identifier,
            loc: loc('6-8'),
            identifier: ident('bar'),
          },
          ExpectedGlyph(Glyph.Comma)
        ),
      ],
      result: ['foo', 'bar'],
    });
  });

  test('missing one comma of two', () => {
    expect(parseCommaListTest(lex('(foo, bar qux)'))).toEqual({
      errors: [
        UnexpectedTokenError(
          {
            type: TokenType.Identifier,
            loc: loc('11-13'),
            identifier: ident('qux'),
          },
          ExpectedGlyph(Glyph.Comma)
        ),
      ],
      result: ['foo', 'bar', 'qux'],
    });
  });

  test('missing two commas of two', () => {
    expect(parseCommaListTest(lex('(foo bar qux)'))).toEqual({
      errors: [
        UnexpectedTokenError(
          {
            type: TokenType.Identifier,
            loc: loc('6-8'),
            identifier: ident('bar'),
          },
          ExpectedGlyph(Glyph.Comma)
        ),
        UnexpectedTokenError(
          {
            type: TokenType.Identifier,
            loc: loc('10-12'),
            identifier: ident('qux'),
          },
          ExpectedGlyph(Glyph.Comma)
        ),
      ],
      result: ['foo', 'bar', 'qux'],
    });
  });

  test('double comma', () => {
    expect(parseCommaListTest(lex('(foo,, bar)'))).toEqual({
      errors: [
        UnexpectedTokenError(
          {type: TokenType.Glyph, loc: loc('6'), glyph: Glyph.Comma},
          ExpectedIdentifier
        ),
      ],
      result: ['foo', 'bar'],
    });
  });

  test('non-comma separator', () => {
    expect(parseCommaListTest(lex('(foo; bar)'))).toEqual({
      errors: [
        UnexpectedTokenError(
          {
            type: TokenType.Glyph,
            loc: loc('5'),
            glyph: Glyph.Semicolon,
          },
          ExpectedGlyph(Glyph.Comma)
        ),
        UnexpectedTokenError(
          {
            type: TokenType.Glyph,
            loc: loc('5'),
            glyph: Glyph.Semicolon,
          },
          ExpectedIdentifier
        ),
      ],
      result: ['foo', 'bar'],
    });
  });

  test('double non-comma separator', () => {
    expect(parseCommaListTest(lex('(foo;; bar)'))).toEqual({
      errors: [
        UnexpectedTokenError(
          {
            type: TokenType.Glyph,
            loc: loc('5'),
            glyph: Glyph.Semicolon,
          },
          ExpectedGlyph(Glyph.Comma)
        ),
        UnexpectedTokenError(
          {
            type: TokenType.Glyph,
            loc: loc('5'),
            glyph: Glyph.Semicolon,
          },
          ExpectedIdentifier
        ),
        UnexpectedTokenError(
          {
            type: TokenType.Glyph,
            loc: loc('6'),
            glyph: Glyph.Semicolon,
          },
          ExpectedIdentifier
        ),
      ],
      result: ['foo', 'bar'],
    });
  });

  test('trailing non-comma separator', () => {
    expect(parseCommaListTest(lex('(foo, bar;)'))).toEqual({
      errors: [
        UnexpectedTokenError(
          {
            type: TokenType.Glyph,
            loc: loc('10'),
            glyph: Glyph.Semicolon,
          },
          ExpectedGlyph(Glyph.Comma)
        ),
        UnexpectedTokenError(
          {
            type: TokenType.Glyph,
            loc: loc('10'),
            glyph: Glyph.Semicolon,
          },
          ExpectedIdentifier
        ),
      ],
      result: ['foo', 'bar'],
    });
  });

  test('not ended when expecting item', () => {
    expect(parseCommaListTest(lex('('))).toEqual({
      errors: [
        UnexpectedTokenError(
          {type: TokenType.End, loc: loc('2')},
          ExpectedIdentifier
        ),
      ],
      result: [],
    });
  });

  test('not ended when expecting comma', () => {
    expect(parseCommaListTest(lex('(foo'))).toEqual({
      errors: [
        UnexpectedTokenError(
          {type: TokenType.End, loc: loc('5')},
          ExpectedGlyph(Glyph.Comma)
        ),
        UnexpectedTokenError(
          {type: TokenType.End, loc: loc('5')},
          ExpectedIdentifier
        ),
      ],
      result: ['foo'],
    });
  });
});
