import {TypeType} from './Ast';
import {ExpectedType, UnexpectedTokenError} from './Error';
import {Identifier} from './Identifier';
import {Glyph, Lexer, TokenType} from './Lexer';
import {Loc, Pos} from './Loc';
import {parseType} from './Parser';

function lex(source: string): Lexer {
  return Lexer.create(source);
}

describe('type', () => {
  test('empty string', () => {
    const loc = new Loc(new Pos(1, 1), new Pos(1, 1));
    const errors = [
      UnexpectedTokenError(
        {type: TokenType.End, loc},
        {type: ExpectedType.Type}
      ),
    ];
    expect(parseType(lex(''))).toEqual({
      errors,
      type: {type: TypeType.Error, loc, error: errors[0]},
    });
  });

  test('invalid string', () => {
    const loc = new Loc(new Pos(1, 1), new Pos(1, 3));
    const errors = [
      UnexpectedTokenError(
        {type: TokenType.Glyph, loc, glyph: Glyph.Ellipsis},
        {type: ExpectedType.Type}
      ),
    ];
    expect(parseType(lex('...'))).toEqual({
      errors,
      type: {
        type: TypeType.Error,
        loc,
        error: errors[0],
      },
    });
  });

  test('binding keyword', () => {
    const loc = new Loc(new Pos(1, 1), new Pos(1, 2));
    const errors = [
      UnexpectedTokenError(
        {type: TokenType.Identifier, loc, identifier: 'if' as Identifier},
        {type: ExpectedType.Type}
      ),
    ];
    expect(parseType(lex('if'))).toEqual({
      errors,
      type: {
        type: TypeType.Error,
        loc,
        error: errors[0],
      },
    });
  });

  test('reference type', () => {
    expect(parseType(lex('foo'))).toEqual({
      errors: [],
      type: {
        type: TypeType.Reference,
        loc: new Loc(new Pos(1, 1), new Pos(1, 3)),
        identifier: 'foo' as Identifier,
      },
    });
  });
});
