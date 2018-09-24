import {Lexer, Glyph, TokenType} from './Lexer';
import {Loc, Pos} from './Loc';
import {Identifier, Keyword} from './Identifier';

describe('glyph', () => {
  [
    ['&', Glyph.Ampersand],
    ['&&', Glyph.AmpersandDouble],
    ['->', Glyph.Arrow],
    ['*', Glyph.Asterisk],
    ['|', Glyph.Bar],
    ['||', Glyph.BarDouble],
    ['{', Glyph.BraceLeft],
    ['}', Glyph.BraceRight],
    ['[', Glyph.BracketLeft],
    [']', Glyph.BracketRight],
    [':', Glyph.Colon],
    [',', Glyph.Comma],
    ['.', Glyph.Dot],
    ['...', Glyph.Ellipsis],
    ['=', Glyph.Equals],
    ['!', Glyph.Exclamation],
    ['>', Glyph.GreaterThan],
    ['>=', Glyph.GreaterThanOrEqual],
    ['<', Glyph.LessThan],
    ['<=', Glyph.LessThanOrEqual],
    ['-', Glyph.Minus],
    ['(', Glyph.ParenLeft],
    [')', Glyph.ParenRight],
    ['%', Glyph.Percent],
    ['+', Glyph.Plus],
    ['?', Glyph.Question],
    [';', Glyph.Semicolon],
    ['/', Glyph.Slash],
    ['\\', Glyph.SlashBackwards],
  ].forEach(([source, glyph]) => {
    test(source, () => {
      const start = Pos.initial();
      const end = new Pos(start.column, start.line + source.length - 1);
      const loc = new Loc(start, end);
      const tokens = [...Lexer.create(source)];
      expect(tokens).toEqual([{type: TokenType.Glyph, loc, glyph}]);
    });
  });

  test('&&&', () => {
    const tokens = [...Lexer.create('&&&')];
    expect(tokens).toEqual([
      {
        type: TokenType.Glyph,
        loc: new Loc(new Pos(1, 1), new Pos(1, 2)),
        glyph: Glyph.AmpersandDouble,
      },
      {
        type: TokenType.Glyph,
        loc: new Loc(new Pos(1, 3), new Pos(1, 3)),
        glyph: Glyph.Ampersand,
      },
    ]);
  });

  test('& & &', () => {
    const tokens = [...Lexer.create('& & &')];
    expect(tokens).toEqual([
      {
        type: TokenType.Glyph,
        loc: new Loc(new Pos(1, 1), new Pos(1, 1)),
        glyph: Glyph.Ampersand,
      },
      {
        type: TokenType.Glyph,
        loc: new Loc(new Pos(1, 3), new Pos(1, 3)),
        glyph: Glyph.Ampersand,
      },
      {
        type: TokenType.Glyph,
        loc: new Loc(new Pos(1, 5), new Pos(1, 5)),
        glyph: Glyph.Ampersand,
      },
    ]);
  });

  test('&!', () => {
    const tokens = [...Lexer.create('&!')];
    expect(tokens).toEqual([
      {
        type: TokenType.Glyph,
        loc: new Loc(new Pos(1, 1), new Pos(1, 1)),
        glyph: Glyph.Ampersand,
      },
      {
        type: TokenType.Glyph,
        loc: new Loc(new Pos(1, 2), new Pos(1, 2)),
        glyph: Glyph.Exclamation,
      },
    ]);
  });

  test('..', () => {
    const tokens = [...Lexer.create('..')];
    expect(tokens).toEqual([
      {
        type: TokenType.Unexpected,
        loc: new Loc(new Pos(1, 2), new Pos(1, 2)),
        unexpected: null,
        expected: '.',
      },
    ]);
  });

  test('..!', () => {
    const tokens = [...Lexer.create('..!')];
    expect(tokens).toEqual([
      {
        type: TokenType.Unexpected,
        loc: new Loc(new Pos(1, 3), new Pos(1, 3)),
        unexpected: '!',
        expected: '.',
      },
    ]);
  });
});

describe('identifier', () => {
  [
    'x',
    'foo',
    'Bar',
    'FooBar',
    'foo_bar',
    '_foo',
    'x1',
    'x123',
    '_1',
    "x'",
    "x''",
    'helloWorld',
  ].forEach(identifier => {
    test(identifier, () => {
      const tokens = [...Lexer.create(identifier)];
      const loc = new Loc(new Pos(1, 1), new Pos(1, identifier.length));
      expect(tokens).toEqual([
        {
          type: TokenType.Identifier,
          loc,
          identifier: Identifier.createAssumingValid(identifier),
        },
      ]);
    });
  });

  [['_', Keyword.Underscore]].forEach(([source, keyword]) => {
    test(source, () => {
      const tokens = [...Lexer.create(source)];
      const loc = new Loc(new Pos(1, 1), new Pos(1, source.length));
      expect(tokens).toEqual([
        {
          type: TokenType.Keyword,
          loc,
          keyword,
        },
      ]);
    });
  });
});
