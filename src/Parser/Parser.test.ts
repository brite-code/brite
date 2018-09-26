import {TypeType} from './Ast';
import {ExpectedType, UnexpectedTokenError} from './Error';
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
          {type: ExpectedType.Type}
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
          {type: ExpectedType.Type}
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
          {type: ExpectedType.Type}
        ),
      ],
      type: undefined,
    });
  });

  test('reference', () => {
    expect(parseType(lex('foo'))).toEqual({
      errors: [],
      type: {
        type: TypeType.Reference,
        loc: loc('1-3'),
        identifier: 'foo' as Identifier,
      },
    });
  });

  test('unit', () => {
    expect(parseType(lex('()'))).toEqual({
      errors: [],
      type: {
        type: TypeType.Unit,
        loc: loc('1-2'),
      },
    });
    expect(parseType(lex('( )'))).toEqual({
      errors: [],
      type: {
        type: TypeType.Unit,
        loc: loc('1-3'),
      },
    });
  });

  test('unit trailing comma', () => {
    expect(parseType(lex('(,)'))).toEqual({
      errors: [
        UnexpectedTokenError(
          {type: TokenType.Glyph, loc: loc('2'), glyph: Glyph.Comma},
          {type: ExpectedType.Type}
        ),
      ],
      type: {
        type: TypeType.Unit,
        loc: loc('1-3'),
      },
    });
  });

  test('wrapped', () => {
    expect(parseType(lex('(foo)'))).toEqual({
      errors: [],
      type: {
        type: TypeType.Wrapped,
        loc: loc('1-5'),
        wrapped: {
          type: TypeType.Reference,
          loc: loc('2-4'),
          identifier: 'foo' as Identifier,
        },
      },
    });
  });

  test('wrapped with trailing comma', () => {
    expect(parseType(lex('(foo,)'))).toEqual({
      errors: [],
      type: {
        type: TypeType.Wrapped,
        loc: loc('1-6'),
        wrapped: {
          type: TypeType.Reference,
          loc: loc('2-4'),
          identifier: 'foo' as Identifier,
        },
      },
    });
  });

  test('tuple with 2 elements', () => {
    expect(parseType(lex('(foo, bar)'))).toEqual({
      errors: [],
      type: {
        type: TypeType.Tuple,
        loc: loc('1-10'),
        elements: [
          {
            type: TypeType.Reference,
            loc: loc('2-4'),
            identifier: 'foo' as Identifier,
          },
          {
            type: TypeType.Reference,
            loc: loc('7-9'),
            identifier: 'bar' as Identifier,
          },
        ],
      },
    });
  });

  test('tuple with 3 elements', () => {
    expect(parseType(lex('(foo, bar, qux)'))).toEqual({
      errors: [],
      type: {
        type: TypeType.Tuple,
        loc: loc('1-15'),
        elements: [
          {
            type: TypeType.Reference,
            loc: loc('2-4'),
            identifier: 'foo' as Identifier,
          },
          {
            type: TypeType.Reference,
            loc: loc('7-9'),
            identifier: 'bar' as Identifier,
          },
          {
            type: TypeType.Reference,
            loc: loc('12-14'),
            identifier: 'qux' as Identifier,
          },
        ],
      },
    });
  });

  test('tuple with 4 elements', () => {
    expect(parseType(lex('(foo, bar, qux, lit)'))).toEqual({
      errors: [],
      type: {
        type: TypeType.Tuple,
        loc: loc('1-20'),
        elements: [
          {
            type: TypeType.Reference,
            loc: loc('2-4'),
            identifier: 'foo' as Identifier,
          },
          {
            type: TypeType.Reference,
            loc: loc('7-9'),
            identifier: 'bar' as Identifier,
          },
          {
            type: TypeType.Reference,
            loc: loc('12-14'),
            identifier: 'qux' as Identifier,
          },
          {
            type: TypeType.Reference,
            loc: loc('17-19'),
            identifier: 'lit' as Identifier,
          },
        ],
      },
    });
  });

  test('record with 0 properties', () => {
    expect(parseType(lex('{}'))).toEqual({
      errors: [],
      type: {
        type: TypeType.Record,
        loc: loc('1-2'),
        properties: [],
      },
    });
  });

  test('record with 1 property', () => {
    expect(parseType(lex('{ foo: T }'))).toEqual({
      errors: [],
      type: {
        type: TypeType.Record,
        loc: loc('1-10'),
        properties: [
          {
            key: {
              loc: loc('3-5'),
              identifier: 'foo' as Identifier,
            },
            value: {
              type: TypeType.Reference,
              loc: loc('8'),
              identifier: 'T' as Identifier,
            },
            optional: false,
          },
        ],
      },
    });
  });

  test('record with 2 properties', () => {
    expect(parseType(lex('{ foo: T, bar: U }'))).toEqual({
      errors: [],
      type: {
        type: TypeType.Record,
        loc: loc('1-18'),
        properties: [
          {
            key: {
              loc: loc('3-5'),
              identifier: 'foo' as Identifier,
            },
            value: {
              type: TypeType.Reference,
              loc: loc('8'),
              identifier: 'T' as Identifier,
            },
            optional: false,
          },
          {
            key: {
              loc: loc('11-13'),
              identifier: 'bar' as Identifier,
            },
            value: {
              type: TypeType.Reference,
              loc: loc('16'),
              identifier: 'U' as Identifier,
            },
            optional: false,
          },
        ],
      },
    });
  });

  test('record with 4 properties', () => {
    expect(parseType(lex('{ foo: T, bar: U, qux: V, lit: W }'))).toEqual({
      errors: [],
      type: {
        type: TypeType.Record,
        loc: loc('1-34'),
        properties: [
          {
            key: {
              loc: loc('3-5'),
              identifier: 'foo' as Identifier,
            },
            value: {
              type: TypeType.Reference,
              loc: loc('8'),
              identifier: 'T' as Identifier,
            },
            optional: false,
          },
          {
            key: {
              loc: loc('11-13'),
              identifier: 'bar' as Identifier,
            },
            value: {
              type: TypeType.Reference,
              loc: loc('16'),
              identifier: 'U' as Identifier,
            },
            optional: false,
          },
          {
            key: {
              loc: loc('19-21'),
              identifier: 'qux' as Identifier,
            },
            value: {
              type: TypeType.Reference,
              loc: loc('24'),
              identifier: 'V' as Identifier,
            },
            optional: false,
          },
          {
            key: {
              loc: loc('27-29'),
              identifier: 'lit' as Identifier,
            },
            value: {
              type: TypeType.Reference,
              loc: loc('32'),
              identifier: 'W' as Identifier,
            },
            optional: false,
          },
        ],
      },
    });
  });

  test('record with optional property', () => {
    expect(parseType(lex('{ foo?: T }'))).toEqual({
      errors: [],
      type: {
        type: TypeType.Record,
        loc: loc('1-11'),
        properties: [
          {
            key: {
              loc: loc('3-5'),
              identifier: 'foo' as Identifier,
            },
            value: {
              type: TypeType.Reference,
              loc: loc('9'),
              identifier: 'T' as Identifier,
            },
            optional: true,
          },
        ],
      },
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
          {type: ExpectedType.Identifier}
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
          {type: ExpectedType.Identifier}
        ),
      ],
      result: [],
    });
    expect(parseCommaListTest(lex('( % )'))).toEqual({
      errors: [
        UnexpectedTokenError(
          {type: TokenType.Glyph, loc: loc('3'), glyph: Glyph.Percent},
          {type: ExpectedType.Identifier}
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
          {type: ExpectedType.Glyph, glyph: Glyph.Comma}
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
          {type: ExpectedType.Glyph, glyph: Glyph.Comma}
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
          {type: ExpectedType.Glyph, glyph: Glyph.Comma}
        ),
        UnexpectedTokenError(
          {
            type: TokenType.Identifier,
            loc: loc('10-12'),
            identifier: 'qux' as Identifier,
          },
          {type: ExpectedType.Glyph, glyph: Glyph.Comma}
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
          {type: ExpectedType.Identifier}
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
          {type: ExpectedType.Glyph, glyph: Glyph.Comma}
        ),
        UnexpectedTokenError(
          {
            type: TokenType.Glyph,
            loc: loc('5'),
            glyph: Glyph.Semicolon,
          },
          {type: ExpectedType.Identifier}
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
          {type: ExpectedType.Glyph, glyph: Glyph.Comma}
        ),
        UnexpectedTokenError(
          {
            type: TokenType.Glyph,
            loc: loc('5'),
            glyph: Glyph.Semicolon,
          },
          {type: ExpectedType.Identifier}
        ),
        UnexpectedTokenError(
          {
            type: TokenType.Glyph,
            loc: loc('6'),
            glyph: Glyph.Semicolon,
          },
          {type: ExpectedType.Identifier}
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
          {type: ExpectedType.Glyph, glyph: Glyph.Comma}
        ),
        UnexpectedTokenError(
          {
            type: TokenType.Glyph,
            loc: loc('10'),
            glyph: Glyph.Semicolon,
          },
          {type: ExpectedType.Identifier}
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
          {type: ExpectedType.Identifier}
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
          {type: ExpectedType.Glyph, glyph: Glyph.Comma}
        ),
        UnexpectedTokenError(
          {type: TokenType.End, loc: loc('5')},
          {type: ExpectedType.Identifier}
        ),
      ],
      result: ['foo'],
    });
  });
});
