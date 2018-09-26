import {
  MemberType,
  Name,
  RecordType,
  RecordTypeProperty,
  ReferenceType,
  TupleType,
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
import {Identifier} from './Identifier';
import {Glyph, Lexer, TokenType} from './Lexer';
import {loc} from './Loc';
import {parseCommaListTest, parseType} from './Parser';

function lex(source: string): Lexer {
  return Lexer.create(source);
}

describe('type', () => {
  test('empty string', () => {
    expect(parseType(lex(''))).toEqual({
      errors: [
        UnexpectedTokenError(
          {type: TokenType.End, loc: loc('1-1')},
          ExpectedType
        ),
      ],
      type: undefined,
    });
  });

  test('invalid string', () => {
    expect(parseType(lex('...'))).toEqual({
      errors: [
        UnexpectedTokenError(
          {type: TokenType.Glyph, loc: loc('1-3'), glyph: Glyph.Ellipsis},
          ExpectedType
        ),
      ],
      type: undefined,
    });
  });

  test('binding keyword', () => {
    expect(parseType(lex('if'))).toEqual({
      errors: [
        UnexpectedTokenError(
          {
            type: TokenType.Identifier,
            loc: loc('1-2'),
            identifier: 'if' as Identifier,
          },
          ExpectedType
        ),
      ],
      type: undefined,
    });
  });

  test('reference', () => {
    expect(parseType(lex('foo'))).toEqual({
      errors: [],
      type: ReferenceType(loc('1-3'), 'foo' as Identifier),
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
      type: WrappedType(
        loc('1-5'),
        ReferenceType(loc('2-4'), 'foo' as Identifier)
      ),
    });
  });

  test('wrapped with trailing comma', () => {
    expect(parseType(lex('(foo,)'))).toEqual({
      errors: [],
      type: WrappedType(
        loc('1-6'),
        ReferenceType(loc('2-4'), 'foo' as Identifier)
      ),
    });
  });

  test('tuple with 2 elements', () => {
    expect(parseType(lex('(foo, bar)'))).toEqual({
      errors: [],
      type: TupleType(loc('1-10'), [
        ReferenceType(loc('2-4'), 'foo' as Identifier),
        ReferenceType(loc('7-9'), 'bar' as Identifier),
      ]),
    });
  });

  test('tuple with 3 elements', () => {
    expect(parseType(lex('(foo, bar, qux)'))).toEqual({
      errors: [],
      type: TupleType(loc('1-15'), [
        ReferenceType(loc('2-4'), 'foo' as Identifier),
        ReferenceType(loc('7-9'), 'bar' as Identifier),
        ReferenceType(loc('12-14'), 'qux' as Identifier),
      ]),
    });
  });

  test('tuple with 4 elements', () => {
    expect(parseType(lex('(foo, bar, qux, lit)'))).toEqual({
      errors: [],
      type: TupleType(loc('1-20'), [
        ReferenceType(loc('2-4'), 'foo' as Identifier),
        ReferenceType(loc('7-9'), 'bar' as Identifier),
        ReferenceType(loc('12-14'), 'qux' as Identifier),
        ReferenceType(loc('17-19'), 'lit' as Identifier),
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
          Name(loc('3-5'), 'foo' as Identifier),
          ReferenceType(loc('8'), 'T' as Identifier)
        ),
      ]),
    });
  });

  test('record with 2 properties', () => {
    expect(parseType(lex('{ foo: T, bar: U }'))).toEqual({
      errors: [],
      type: RecordType(loc('1-18'), [
        RecordTypeProperty(
          Name(loc('3-5'), 'foo' as Identifier),
          ReferenceType(loc('8'), 'T' as Identifier)
        ),
        RecordTypeProperty(
          Name(loc('11-13'), 'bar' as Identifier),
          ReferenceType(loc('16'), 'U' as Identifier)
        ),
      ]),
    });
  });

  test('record with 4 properties', () => {
    expect(parseType(lex('{ foo: T, bar: U, qux: V, lit: W }'))).toEqual({
      errors: [],
      type: RecordType(loc('1-34'), [
        RecordTypeProperty(
          Name(loc('3-5'), 'foo' as Identifier),
          ReferenceType(loc('8'), 'T' as Identifier)
        ),
        RecordTypeProperty(
          Name(loc('11-13'), 'bar' as Identifier),
          ReferenceType(loc('16'), 'U' as Identifier)
        ),
        RecordTypeProperty(
          Name(loc('19-21'), 'qux' as Identifier),
          ReferenceType(loc('24'), 'V' as Identifier)
        ),
        RecordTypeProperty(
          Name(loc('27-29'), 'lit' as Identifier),
          ReferenceType(loc('32'), 'W' as Identifier)
        ),
      ]),
    });
  });

  test('record with optional property', () => {
    expect(parseType(lex('{ foo?: T }'))).toEqual({
      errors: [],
      type: RecordType(loc('1-11'), [
        RecordTypeProperty.optional(
          Name(loc('3-5'), 'foo' as Identifier),
          ReferenceType(loc('9'), 'T' as Identifier)
        ),
      ]),
    });
  });

  test('member', () => {
    expect(parseType(lex('hello.world'))).toEqual({
      errors: [],
      type: MemberType(
        loc('1-11'),
        ReferenceType(loc('1-5'), 'hello' as Identifier),
        Name(loc('7-11'), 'world' as Identifier)
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
          ReferenceType(loc('1-3'), 'foo' as Identifier),
          Name(loc('5-7'), 'bar' as Identifier)
        ),
        Name(loc('9-11'), 'qux' as Identifier)
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
            ReferenceType(loc('1-3'), 'foo' as Identifier),
            Name(loc('5-7'), 'bar' as Identifier)
          ),
          Name(loc('9-11'), 'qux' as Identifier)
        ),
        Name(loc('13-15'), 'lit' as Identifier)
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
      type: ReferenceType(loc('1-5'), 'hello' as Identifier),
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
      type: ReferenceType(loc('1-5'), 'hello' as Identifier),
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
            identifier: 'bar' as Identifier,
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
            identifier: 'qux' as Identifier,
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
            identifier: 'bar' as Identifier,
          },
          ExpectedGlyph(Glyph.Comma)
        ),
        UnexpectedTokenError(
          {
            type: TokenType.Identifier,
            loc: loc('10-12'),
            identifier: 'qux' as Identifier,
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
