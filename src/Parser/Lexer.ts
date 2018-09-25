import {ResultType} from '../Result';

import {Identifier, Keyword} from './Identifier';
import {Loc, Pos} from './Loc';

/**
 * The type for `Token`.
 */
export const enum TokenType {
  Identifier = 'Identifier',
  Keyword = 'Keyword',
  Glyph = 'Glyph',
  Unexpected = 'Unexpected',
}

/**
 * The type for the domain specific tokens that Brite turns source text into.
 */
export type Token =
  | IdentifierToken
  | KeywordToken
  | GlyphToken
  | UnexpectedToken;

/**
 * An identifier token represents the name of some Brite construct.
 *
 * Some words are used in the parser to to denote a specific kind of Brite
 * construct. Like `type`. Some of these words are reserved as keywords, but
 * others are only used in non-ambiguous settings.
 */
export type IdentifierToken = {
  readonly type: TokenType.Identifier;
  readonly loc: Loc;
  readonly identifier: Identifier;
};

/**
 * A keyword is a reserved identifier. Despite being valid identifiers, keywords
 * are reserved for their use as syntax.
 */
export type KeywordToken = {
  readonly type: TokenType.Keyword;
  readonly loc: Loc;
  readonly keyword: Keyword;
};

/**
 * A glyph is some character or characters which represents, graphically, some
 * behavior of the program. For example, the equals glyph (`=`) graphically
 * represents setting variable name to a value.
 */
export const enum Glyph {
  Ampersand = '&',
  AmpersandDouble = '&&',
  Arrow = '->',
  Asterisk = '*',
  Bar = '|',
  BarDouble = '||',
  BraceLeft = '{',
  BraceRight = '}',
  BracketLeft = '[',
  BracketRight = ']',
  Colon = ':',
  Comma = ',',
  Dot = '.',
  Ellipsis = '...',
  Equals = '=',
  Exclamation = '!',
  GreaterThan = '>',
  GreaterThanOrEqual = '>=',
  LessThan = '<',
  LessThanOrEqual = '<=',
  Minus = '-',
  ParenLeft = '(',
  ParenRight = ')',
  Percent = '%',
  Plus = '+',
  Question = '?',
  Semicolon = ';',
  Slash = '/',
  SlashBackwards = '\\',
}

/**
 * A token which holds a glyph.
 */
export type GlyphToken = {
  readonly type: TokenType.Glyph;
  readonly loc: Loc;
  readonly glyph: Glyph;
};

/**
 * Sometimes during parsing we encounter a character that we did not expect. In
 * this case we insert an unexpected token. If the parser expected a specific
 * character then we include that in our token.
 *
 * The parser is designed to be resistent to unexpected tokens so it should be
 * able to continue parsing even in the face of one of these. If there is no
 * unexpected character we arrived at an unexpected ending.
 */
export type UnexpectedToken = {
  readonly type: TokenType.Unexpected;
  readonly loc: Loc;
  readonly unexpected: string | undefined;
  readonly expected: string | undefined | false;
};

/**
 * A Brite source program starts its life as a sequence of Unicode characters
 * (graphemes). The lexer is responsible for turning that program into a
 * sequence of domain specific tokens.
 */
export class Lexer implements Iterator<Token>, Iterable<Token> {
  /**
   * Creates a new lexer from a source text string.
   */
  static create(source: string): Lexer {
    return new Lexer(Pos.initial(), peekable(source[Symbol.iterator]()));
  }

  // We store the line and column as mutable fields on `Lexer` so that we aren’t
  // allocating memory for a `Pos` on every character.
  private line: number;
  private column: number;

  private readonly chars: PeekableIterator<string>;

  private constructor(pos: Pos, chars: PeekableIterator<string>) {
    this.line = pos.line;
    this.column = pos.column - 1;
    this.chars = chars;
  }

  [Symbol.iterator](): Lexer {
    return this;
  }

  next(): IteratorResult<Token> {
    const g = (glyph: Glyph): IteratorResult<Token> => ({
      done: false,
      value: {type: TokenType.Glyph, loc: this.currentLoc(), glyph},
    });
    const g2 = (loc: Loc, glyph: Glyph): IteratorResult<Token> => ({
      done: false,
      value: {type: TokenType.Glyph, loc, glyph},
    });

    const c = this.nextChar();
    switch (c) {
      // Parse any glyph that we can.

      case '*':
        return g(Glyph.Asterisk);
      case '{':
        return g(Glyph.BraceLeft);
      case '}':
        return g(Glyph.BraceRight);
      case '[':
        return g(Glyph.BracketLeft);
      case ']':
        return g(Glyph.BracketRight);
      case ':':
        return g(Glyph.Colon);
      case ',':
        return g(Glyph.Comma);
      case '=':
        return g(Glyph.Equals);
      case '!':
        return g(Glyph.Exclamation);
      case '(':
        return g(Glyph.ParenLeft);
      case ')':
        return g(Glyph.ParenRight);
      case '%':
        return g(Glyph.Percent);
      case '+':
        return g(Glyph.Plus);
      case '?':
        return g(Glyph.Question);
      case ';':
        return g(Glyph.Semicolon);
      case '/':
        return g(Glyph.Slash);
      case '\\':
        return g(Glyph.SlashBackwards);

      case '&': {
        if (this.peekChar() === '&') {
          const start = this.currentPos();
          this.nextChar();
          const end = this.currentPos();
          const loc = new Loc(start, end);
          return g2(loc, Glyph.AmpersandDouble);
        }
        return g(Glyph.Ampersand);
      }

      case '|': {
        if (this.peekChar() === '|') {
          const start = this.currentPos();
          this.nextChar();
          const end = this.currentPos();
          const loc = new Loc(start, end);
          return g2(loc, Glyph.BarDouble);
        }
        return g(Glyph.Bar);
      }

      case '.': {
        if (this.peekChar() === '.') {
          const start = this.currentPos();
          this.nextChar();
          const thirdChar = this.nextChar();
          if (thirdChar === '.') {
            const end = this.currentPos();
            const loc = new Loc(start, end);
            return g2(loc, Glyph.Ellipsis);
          } else {
            return {
              done: false,
              value: {
                type: TokenType.Unexpected,
                loc: this.currentLoc(),
                unexpected: thirdChar,
                expected: '.',
              },
            };
          }
        }
        return g(Glyph.Dot);
      }

      case '>': {
        if (this.peekChar() === '=') {
          const start = this.currentPos();
          this.nextChar();
          const end = this.currentPos();
          const loc = new Loc(start, end);
          return g2(loc, Glyph.GreaterThanOrEqual);
        }
        return g(Glyph.GreaterThan);
      }

      case '<': {
        if (this.peekChar() === '=') {
          const start = this.currentPos();
          this.nextChar();
          const end = this.currentPos();
          const loc = new Loc(start, end);
          return g2(loc, Glyph.LessThanOrEqual);
        }
        return g(Glyph.LessThan);
      }

      case '-': {
        if (this.peekChar() === '>') {
          const start = this.currentPos();
          this.nextChar();
          const end = this.currentPos();
          const loc = new Loc(start, end);
          return g2(loc, Glyph.Arrow);
        }
        return g(Glyph.Minus);
      }

      // TODO: Proper `White_space` category check.
      case ' ':
      case '\n':
      case '\t':
      case '\r': {
        return this.next();
      }

      // If we have no more characters then we are done!
      case undefined:
        return {done: true, value: undefined as never};

      default: {
        if (Identifier.isStart(c)) {
          // Parse an identifier according to our identifier specification.

          const start = this.currentPos();
          let identifier = c;

          // Parse any identifier continuing characters.
          while (true) {
            const c = this.peekChar();
            if (c !== undefined && Identifier.isContinue(c)) {
              identifier += c;
              this.nextChar();
            } else {
              break;
            }
          }

          // Parse any identifier finishing characters.
          while (true) {
            const c = this.peekChar();
            if (c !== undefined && Identifier.isFinish(c)) {
              identifier += c;
              this.nextChar();
            } else {
              break;
            }
          }

          // Finally, create an identifier. Assume we have the right syntax, but
          // we still want to check for keywords.
          const end = this.currentPos();
          const loc = new Loc(start, end);
          const result = Identifier.createAssumingValidSyntax(identifier);
          switch (result.type) {
            case ResultType.Ok: {
              return {
                done: false,
                value: {
                  type: TokenType.Identifier,
                  loc,
                  identifier: result.value,
                },
              };
            }
            case ResultType.Err: {
              return {
                done: false,
                value: {
                  type: TokenType.Keyword,
                  loc,
                  keyword: result.value,
                },
              };
            }
            default:
              throw new Error('unreachable');
          }
        } else {
          // We found an unexpected character! Return an unexpected token error.
          return {
            done: false,
            value: {
              type: TokenType.Unexpected,
              loc: this.currentLoc(),
              unexpected: c,
              expected: false,
            },
          };
        }
      }
    }
  }

  /**
   * Moves the lexer state to the next character and returns that character.
   * Returns undefined if there are no more characters. Updates the
   * lexer position.
   */
  private nextChar(): string | undefined {
    // NOTE: This function should be protected in Brite. So it’s only accessible
    // in this namespace.
    const step = this.chars.next();
    if (step.done) {
      return undefined;
    }
    if (step.value === '\n') {
      this.line += 1;
      this.column = 0;
    } else {
      this.column += 1;
    }
    return step.value;
  }

  /**
   * Peeks the next character we are about to parse and returns that character.
   * Returns undefined if there are no more characters.
   */
  private peekChar(): string | undefined {
    // NOTE: This function should be protected in Brite. So it’s only accessible
    // in this namespace.
    const step = this.chars.peek();
    return step.done ? undefined : step.value;
  }

  private currentPos(): Pos {
    return new Pos(this.line, this.column);
  }

  private currentLoc(): Loc {
    const pos = this.currentPos();
    return new Loc(pos, pos);
  }
}

/**
 * A peekable iterator allows you to peek the next item without consuming it.
 * Note that peekable iterators do not allow you to pass in a value with
 * `next()`. The interface of `PekableIterator<T>` is much more limited then the
 * interface of `Iterator<T>` to allow for peeking.
 */
interface PeekableIterator<T> {
  next(): IteratorResult<T>;
  peek(): IteratorResult<T>;
}

/**
 * Turns an iterator into a peekable iterator.
 */
function peekable<T>(iterator: Iterator<T>): PeekableIterator<T> {
  let peeking: IteratorResult<T> | undefined = undefined;
  return {
    next: () => {
      if (peeking !== undefined) {
        const next = peeking;
        peeking = undefined;
        return next;
      } else {
        return iterator.next();
      }
    },
    peek: () => {
      if (peeking === undefined) {
        peeking = iterator.next();
      }
      return peeking;
    },
  };
}
