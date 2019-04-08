import {Err, Ok} from '../Utils/Result';

import {
  ExpectedGlyph,
  ExpectedIdentifier,
  ExpectedLineSeparator,
  UnexpectedTokenError,
} from './Error';
import {ident} from './Identifier';
import {EndToken, Glyph, GlyphToken, IdentifierToken, Lexer} from './Lexer';
import {loc} from './Loc';
import {parseCommaListTest, parseLineSeparatorListTest} from './Parser';

function lex(source: string): Lexer {
  return Lexer.create(source);
}

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

describe('line separator list', () => {
  [
    {
      source: '()',
      result: Ok([]),
    },
    {
      source: '(foo)',
      result: Ok(['foo']),
    },
    {
      source: '(foo; bar)',
      result: Ok(['foo', 'bar']),
    },
    {
      source: '(foo; bar; qux; lit)',
      result: Ok(['foo', 'bar', 'qux', 'lit']),
    },
    {
      source: '(;)',
      result: Err(
        UnexpectedTokenError(
          GlyphToken(loc('2'), Glyph.Semicolon),
          ExpectedIdentifier
        )
      ),
    },
    {
      source: '(foo;)',
      result: Ok(['foo']),
    },
    {
      source: '(foo; bar;)',
      result: Ok(['foo', 'bar']),
    },
    {
      source: '(foo; bar; qux; lit;)',
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
          ExpectedLineSeparator
        )
      ),
    },
    {
      source: '(foo; bar qux)',
      result: Err(
        UnexpectedTokenError(
          IdentifierToken(loc('11-13'), ident('qux')),
          ExpectedLineSeparator
        )
      ),
    },
    {
      source: '(foo bar qux)',
      result: Err(
        UnexpectedTokenError(
          IdentifierToken(loc('6-8'), ident('bar')),
          ExpectedLineSeparator
        )
      ),
    },
    {
      source: '(foo;; bar)',
      result: Err(
        UnexpectedTokenError(
          GlyphToken(loc('6'), Glyph.Semicolon),
          ExpectedIdentifier
        )
      ),
    },
    {
      source: '(foo, bar)',
      result: Err(
        UnexpectedTokenError(
          GlyphToken(loc('5'), Glyph.Comma),
          ExpectedLineSeparator
        )
      ),
    },
    {
      source: '(foo,, bar)',
      result: Err(
        UnexpectedTokenError(
          GlyphToken(loc('5'), Glyph.Comma),
          ExpectedLineSeparator
        )
      ),
    },
    {
      source: '(foo; bar,)',
      result: Err(
        UnexpectedTokenError(
          GlyphToken(loc('10'), Glyph.Comma),
          ExpectedLineSeparator
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
        UnexpectedTokenError(EndToken(loc('5')), ExpectedLineSeparator)
      ),
    },
    {
      source: '(\n)',
      result: Ok([]),
    },
    {
      source: '(foo\n)',
      result: Ok(['foo']),
    },
    {
      source: '(foo\nbar)',
      result: Ok(['foo', 'bar']),
    },
    {
      source: '(\n\n)',
      result: Ok([]),
    },
    {
      source: '(\nfoo\n)',
      result: Ok(['foo']),
    },
    {
      source: '(foo\n\n)',
      result: Ok(['foo']),
    },
    {
      source: '(foo\n\nbar)',
      result: Ok(['foo', 'bar']),
    },
    {
      source: '(\n  foo\n  bar\n  qux\n  lit\n)',
      result: Ok(['foo', 'bar', 'qux', 'lit']),
    },
    {
      source: '(foo;\nbar)',
      result: Ok(['foo', 'bar']),
    },
    {
      source: '(foo\n;bar)',
      result: Ok(['foo', 'bar']),
    },
  ].forEach(({source, result}) => {
    test(source.replace(/\n/g, '\\n'), () => {
      expect(parseLineSeparatorListTest(lex(source))).toEqual(result);
    });
  });
});
