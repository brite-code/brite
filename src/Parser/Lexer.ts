import {ResultType} from '../Utils/Result';

import {Identifier, Keyword} from './Identifier';
import {Loc, Pos} from './Loc';

/**
 * The type for the domain specific tokens that Brite turns source text into.
 */
export type Token =
  | IdentifierToken
  | KeywordToken
  | GlyphToken
  | UnexpectedCharToken
  | EndToken;

/**
 * The type for `Token`.
 */
export const enum TokenType {
  Identifier = 'Identifier',
  Keyword = 'Keyword',
  Glyph = 'Glyph',
  Unexpected = 'Unexpected',
  End = 'End',
}

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
  Assignment = ':=',
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
  EqualsDouble = '==',
  Exclamation = '!',
  FatArrow = '=>',
  GreaterThan = '>',
  GreaterThanOrEqual = '>=',
  LessThan = '<',
  LessThanOrEqual = '<=',
  Minus = '-',
  NotEquals = '!=',
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
export type UnexpectedCharToken = {
  readonly type: TokenType.Unexpected;
  readonly loc: Loc;
  readonly unexpected: string | undefined;
  readonly expected: string | undefined | false;
};

/**
 * The last token in the file. Once the iterator stops we emit this token and
 * keep emitting it.
 */
export type EndToken = {
  readonly type: TokenType.End;
  readonly loc: Loc;
};

/**
 * A Brite source program starts its life as a sequence of Unicode characters
 * (graphemes). The lexer is responsible for turning that program into a
 * sequence of domain specific tokens.
 */
export class Lexer implements Iterable<Token> {
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

  private peeked: Token | undefined;
  private peeks = 0;

  private readonly chars: PeekableIterator<string>;

  private constructor(pos: Pos, chars: PeekableIterator<string>) {
    this.line = pos.line;
    this.column = pos.column - 1;
    this.chars = chars;
  }

  [Symbol.iterator](): Iterator<Token> {
    return {
      next: () => {
        const token = this.next();
        if (token.type === TokenType.End) {
          return {done: true, value: token};
        } else {
          return {done: false, value: token};
        }
      },
    };
  }

  /**
   * Look ahead at the next token without advancing the lexer.
   */
  peek(): Token {
    // Defend against infinite loops.
    this.peeks++;
    if (this.peeks >= 50) {
      throw new Error(
        '`Lexer.peek()` called 50 times without calling `Lexer.next()`. ' +
          'The parser is probably stuck in an infinite loop.'
      );
    }

    if (this.peeked === undefined) {
      this.peeked = this.next();
    }

    return this.peeked;
  }

  /**
   * Advance the lexer and return the current token.
   */
  next(): Token {
    if (this.peeked !== undefined) {
      const peeked = this.peeked;
      this.peeked = undefined;
      this.peeks = 0;
      return peeked;
    }

    const g = (glyph: Glyph): Token => ({
      type: TokenType.Glyph,
      loc: this.currentLoc(),
      glyph,
    });
    const g2 = (loc: Loc, glyph: Glyph): Token => ({
      type: TokenType.Glyph,
      loc,
      glyph,
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
      case ',':
        return g(Glyph.Comma);
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

      case ':': {
        if (this.peekChar() === '=') {
          const start = this.currentPos();
          this.nextChar();
          const end = this.currentPos();
          const loc = new Loc(start, end);
          return g2(loc, Glyph.Assignment);
        }
        return g(Glyph.Colon);
      }

      case '=': {
        if (this.peekChar() === '=') {
          const start = this.currentPos();
          this.nextChar();
          const end = this.currentPos();
          const loc = new Loc(start, end);
          return g2(loc, Glyph.EqualsDouble);
        }
        if (this.peekChar() === '>') {
          const start = this.currentPos();
          this.nextChar();
          const end = this.currentPos();
          const loc = new Loc(start, end);
          return g2(loc, Glyph.FatArrow);
        }
        return g(Glyph.Equals);
      }

      case '!': {
        if (this.peekChar() === '=') {
          const start = this.currentPos();
          this.nextChar();
          const end = this.currentPos();
          const loc = new Loc(start, end);
          return g2(loc, Glyph.NotEquals);
        }
        return g(Glyph.Exclamation);
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
              type: TokenType.Unexpected,
              loc: this.currentLoc(),
              unexpected: thirdChar,
              expected: '.',
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
      case undefined: {
        return {
          type: TokenType.End,
          loc: this.currentLoc(),
        };
      }

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
                type: TokenType.Identifier,
                loc,
                identifier: result.value,
              };
            }
            case ResultType.Err: {
              return {
                type: TokenType.Keyword,
                loc,
                keyword: result.value,
              };
            }
            default:
              throw new Error('unreachable');
          }
        } else {
          // We found an unexpected character! Return an unexpected token error.
          return {
            type: TokenType.Unexpected,
            loc: this.currentLoc(),
            unexpected: c,
            expected: false,
          };
        }
      }
    }
  }

  /**
   * The current location of the lexer. If the lexer has completed then this
   * points to the last location of the lexer.
   */
  private currentLoc(): Loc {
    const pos = this.currentPos();
    return new Loc(pos, pos);
  }

  /**
   * The current position of the lexer. If the lexer has completed then this
   * points to the last position of the lexer.
   */
  private currentPos(): Pos {
    return new Pos(this.line, this.column);
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
      this.column += 1;
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
