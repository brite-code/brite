import {TypeType} from './Ast';
import {ExpectedType, UnexpectedTokenError} from './Error';
import {Identifier} from './Identifier';
import {Glyph, Lexer, TokenType} from './Lexer';
import {Loc, Pos} from './Loc';
import {parseCommaListTest, parseType} from './Parser';

function lex(source: string): Lexer {
  return Lexer.create(source);
}

describe('type', () => {
  test('empty string', () => {
    const loc = new Loc(new Pos(1, 1), new Pos(1, 1));
    expect(parseType(lex(''))).toEqual({
      errors: [
        UnexpectedTokenError(
          {type: TokenType.End, loc},
          {type: ExpectedType.Type}
        ),
      ],
      type: undefined,
    });
  });

  test('invalid string', () => {
    const loc = new Loc(new Pos(1, 1), new Pos(1, 3));
    expect(parseType(lex('...'))).toEqual({
      errors: [
        UnexpectedTokenError(
          {type: TokenType.Glyph, loc, glyph: Glyph.Ellipsis},
          {type: ExpectedType.Type}
        ),
      ],
      type: undefined,
    });
  });

  test('binding keyword', () => {
    const loc = new Loc(new Pos(1, 1), new Pos(1, 2));
    expect(parseType(lex('if'))).toEqual({
      errors: [
        UnexpectedTokenError(
          {type: TokenType.Identifier, loc, identifier: 'if' as Identifier},
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
        loc: new Loc(new Pos(1, 1), new Pos(1, 3)),
        identifier: 'foo' as Identifier,
      },
    });
  });

  test('unit', () => {
    expect(parseType(lex('()'))).toEqual({
      errors: [],
      type: {
        type: TypeType.Unit,
        loc: new Loc(new Pos(1, 1), new Pos(1, 2)),
      },
    });
    expect(parseType(lex('( )'))).toEqual({
      errors: [],
      type: {
        type: TypeType.Unit,
        loc: new Loc(new Pos(1, 1), new Pos(1, 3)),
      },
    });
  });

  test('unit trailing comma', () => {
    expect(parseType(lex('(,)'))).toEqual({
      errors: [
        UnexpectedTokenError(
          {type: TokenType.Glyph, loc: Loc.pos(1, 2), glyph: Glyph.Comma},
          {type: ExpectedType.Type}
        ),
      ],
      type: {
        type: TypeType.Unit,
        loc: new Loc(new Pos(1, 1), new Pos(1, 3)),
      },
    });
  });

  test('wrapped', () => {
    expect(parseType(lex('(foo)'))).toEqual({
      errors: [],
      type: {
        type: TypeType.Wrapped,
        loc: new Loc(new Pos(1, 1), new Pos(1, 5)),
        wrapped: {
          type: TypeType.Reference,
          loc: new Loc(new Pos(1, 2), new Pos(1, 4)),
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
        loc: new Loc(new Pos(1, 1), new Pos(1, 6)),
        wrapped: {
          type: TypeType.Reference,
          loc: new Loc(new Pos(1, 2), new Pos(1, 4)),
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
        loc: new Loc(new Pos(1, 1), new Pos(1, 10)),
        elements: [
          {
            type: TypeType.Reference,
            loc: new Loc(new Pos(1, 2), new Pos(1, 4)),
            identifier: 'foo' as Identifier,
          },
          {
            type: TypeType.Reference,
            loc: new Loc(new Pos(1, 7), new Pos(1, 9)),
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
        loc: new Loc(new Pos(1, 1), new Pos(1, 15)),
        elements: [
          {
            type: TypeType.Reference,
            loc: new Loc(new Pos(1, 2), new Pos(1, 4)),
            identifier: 'foo' as Identifier,
          },
          {
            type: TypeType.Reference,
            loc: new Loc(new Pos(1, 7), new Pos(1, 9)),
            identifier: 'bar' as Identifier,
          },
          {
            type: TypeType.Reference,
            loc: new Loc(new Pos(1, 12), new Pos(1, 14)),
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
        loc: new Loc(new Pos(1, 1), new Pos(1, 20)),
        elements: [
          {
            type: TypeType.Reference,
            loc: new Loc(new Pos(1, 2), new Pos(1, 4)),
            identifier: 'foo' as Identifier,
          },
          {
            type: TypeType.Reference,
            loc: new Loc(new Pos(1, 7), new Pos(1, 9)),
            identifier: 'bar' as Identifier,
          },
          {
            type: TypeType.Reference,
            loc: new Loc(new Pos(1, 12), new Pos(1, 14)),
            identifier: 'qux' as Identifier,
          },
          {
            type: TypeType.Reference,
            loc: new Loc(new Pos(1, 17), new Pos(1, 19)),
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
        loc: new Loc(new Pos(1, 1), new Pos(1, 2)),
        properties: [],
      },
    });
  });

  test('record with 1 property', () => {
    expect(parseType(lex('{ foo: T }'))).toEqual({
      errors: [],
      type: {
        type: TypeType.Record,
        loc: new Loc(new Pos(1, 1), new Pos(1, 10)),
        properties: [
          {
            key: {
              loc: new Loc(new Pos(1, 3), new Pos(1, 5)),
              identifier: 'foo' as Identifier,
            },
            value: {
              type: TypeType.Reference,
              loc: Loc.pos(1, 8),
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
        loc: new Loc(new Pos(1, 1), new Pos(1, 18)),
        properties: [
          {
            key: {
              loc: new Loc(new Pos(1, 3), new Pos(1, 5)),
              identifier: 'foo' as Identifier,
            },
            value: {
              type: TypeType.Reference,
              loc: Loc.pos(1, 8),
              identifier: 'T' as Identifier,
            },
            optional: false,
          },
          {
            key: {
              loc: new Loc(new Pos(1, 11), new Pos(1, 13)),
              identifier: 'bar' as Identifier,
            },
            value: {
              type: TypeType.Reference,
              loc: Loc.pos(1, 16),
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
        loc: new Loc(new Pos(1, 1), new Pos(1, 34)),
        properties: [
          {
            key: {
              loc: new Loc(new Pos(1, 3), new Pos(1, 5)),
              identifier: 'foo' as Identifier,
            },
            value: {
              type: TypeType.Reference,
              loc: Loc.pos(1, 8),
              identifier: 'T' as Identifier,
            },
            optional: false,
          },
          {
            key: {
              loc: new Loc(new Pos(1, 11), new Pos(1, 13)),
              identifier: 'bar' as Identifier,
            },
            value: {
              type: TypeType.Reference,
              loc: Loc.pos(1, 16),
              identifier: 'U' as Identifier,
            },
            optional: false,
          },
          {
            key: {
              loc: new Loc(new Pos(1, 19), new Pos(1, 21)),
              identifier: 'qux' as Identifier,
            },
            value: {
              type: TypeType.Reference,
              loc: Loc.pos(1, 24),
              identifier: 'V' as Identifier,
            },
            optional: false,
          },
          {
            key: {
              loc: new Loc(new Pos(1, 27), new Pos(1, 29)),
              identifier: 'lit' as Identifier,
            },
            value: {
              type: TypeType.Reference,
              loc: Loc.pos(1, 32),
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
        loc: new Loc(new Pos(1, 1), new Pos(1, 11)),
        properties: [
          {
            key: {
              loc: new Loc(new Pos(1, 3), new Pos(1, 5)),
              identifier: 'foo' as Identifier,
            },
            value: {
              type: TypeType.Reference,
              loc: Loc.pos(1, 9),
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
          {type: TokenType.Glyph, loc: Loc.pos(1, 2), glyph: Glyph.Comma},
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
          {type: TokenType.Glyph, loc: Loc.pos(1, 2), glyph: Glyph.Percent},
          {type: ExpectedType.Identifier}
        ),
      ],
      result: [],
    });
    expect(parseCommaListTest(lex('( % )'))).toEqual({
      errors: [
        UnexpectedTokenError(
          {type: TokenType.Glyph, loc: Loc.pos(1, 3), glyph: Glyph.Percent},
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
            loc: new Loc(new Pos(1, 6), new Pos(1, 8)),
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
            loc: new Loc(new Pos(1, 11), new Pos(1, 13)),
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
            loc: new Loc(new Pos(1, 6), new Pos(1, 8)),
            identifier: 'bar' as Identifier,
          },
          {type: ExpectedType.Glyph, glyph: Glyph.Comma}
        ),
        UnexpectedTokenError(
          {
            type: TokenType.Identifier,
            loc: new Loc(new Pos(1, 10), new Pos(1, 12)),
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
          {type: TokenType.Glyph, loc: Loc.pos(1, 6), glyph: Glyph.Comma},
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
          {type: TokenType.Glyph, loc: Loc.pos(1, 5), glyph: Glyph.Semicolon},
          {type: ExpectedType.Glyph, glyph: Glyph.Comma}
        ),
        UnexpectedTokenError(
          {type: TokenType.Glyph, loc: Loc.pos(1, 5), glyph: Glyph.Semicolon},
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
          {type: TokenType.Glyph, loc: Loc.pos(1, 5), glyph: Glyph.Semicolon},
          {type: ExpectedType.Glyph, glyph: Glyph.Comma}
        ),
        UnexpectedTokenError(
          {type: TokenType.Glyph, loc: Loc.pos(1, 5), glyph: Glyph.Semicolon},
          {type: ExpectedType.Identifier}
        ),
        UnexpectedTokenError(
          {type: TokenType.Glyph, loc: Loc.pos(1, 6), glyph: Glyph.Semicolon},
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
          {type: TokenType.Glyph, loc: Loc.pos(1, 10), glyph: Glyph.Semicolon},
          {type: ExpectedType.Glyph, glyph: Glyph.Comma}
        ),
        UnexpectedTokenError(
          {type: TokenType.Glyph, loc: Loc.pos(1, 10), glyph: Glyph.Semicolon},
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
          {type: TokenType.End, loc: Loc.pos(1, 2)},
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
          {type: TokenType.End, loc: Loc.pos(1, 5)},
          {type: ExpectedType.Glyph, glyph: Glyph.Comma}
        ),
        UnexpectedTokenError(
          {type: TokenType.End, loc: Loc.pos(1, 5)},
          {type: ExpectedType.Identifier}
        ),
      ],
      result: ['foo'],
    });
  });
});
