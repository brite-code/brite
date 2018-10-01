import {Keyword, ident} from './Identifier';
import {
  Glyph,
  GlyphToken,
  IdentifierToken,
  KeywordToken,
  Lexer,
  UnexpectedCharToken,
} from './Lexer';
import {Loc, Pos, loc} from './Loc';

describe('glyph', () => {
  [
    ['&', Glyph.Ampersand],
    ['&&', Glyph.AmpersandDouble],
    ['->', Glyph.Arrow],
    [':=', Glyph.Assignment],
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
    ['==', Glyph.EqualsDouble],
    ['!', Glyph.Exclamation],
    ['=>', Glyph.FatArrow],
    ['>', Glyph.GreaterThan],
    ['>=', Glyph.GreaterThanOrEqual],
    ['<', Glyph.LessThan],
    ['<=', Glyph.LessThanOrEqual],
    ['-', Glyph.Minus],
    ['!=', Glyph.NotEquals],
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
      expect(tokens).toEqual([GlyphToken(loc, glyph as Glyph)]);
    });
  });

  test('&&&', () => {
    const tokens = [...Lexer.create('&&&')];
    expect(tokens).toEqual([
      GlyphToken(loc('1-2'), Glyph.AmpersandDouble),
      GlyphToken(loc('3'), Glyph.Ampersand),
    ]);
  });

  test('& & &', () => {
    const tokens = [...Lexer.create('& & &')];
    expect(tokens).toEqual([
      GlyphToken(loc('1'), Glyph.Ampersand),
      GlyphToken(loc('3'), Glyph.Ampersand),
      GlyphToken(loc('5'), Glyph.Ampersand),
    ]);
  });

  test('&!', () => {
    const tokens = [...Lexer.create('&!')];
    expect(tokens).toEqual([
      GlyphToken(loc('1'), Glyph.Ampersand),
      GlyphToken(loc('2'), Glyph.Exclamation),
    ]);
  });

  test('..', () => {
    const tokens = [...Lexer.create('..')];
    expect(tokens).toEqual([UnexpectedCharToken(loc('3'), undefined, '.')]);
  });

  test('..!', () => {
    const tokens = [...Lexer.create('..!')];
    expect(tokens).toEqual([UnexpectedCharToken(loc('3'), '!', '.')]);
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
      expect(tokens).toEqual([IdentifierToken(loc, ident(identifier))]);
    });
  });

  [['_', Keyword.Underscore]].forEach(([source, keyword]) => {
    test(source, () => {
      const tokens = [...Lexer.create(source)];
      const loc = new Loc(new Pos(1, 1), new Pos(1, source.length));
      expect(tokens).toEqual([KeywordToken(loc, keyword as Keyword)]);
    });
  });
});
