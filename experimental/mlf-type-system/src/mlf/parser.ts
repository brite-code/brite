import {Expression} from './expression';
import {Monotype, Polytype, Type} from './type';

/**
 * Parses an expression from the provided source string.
 */
export function parseExpression(source: string): Expression {
  const lexer = new Lexer(source);
  const parser = new Parser(lexer);
  const expression = parser.parseExpression();
  if (lexer.next().kind !== 'End') throw new Error('Expected ending.');
  return expression;
}

/**
 * Parses a type from the provided source string.
 */
export function parseType(source: string): Type {
  const lexer = new Lexer(source);
  const parser = new Parser(lexer);
  const type = parser.parsePolytype();
  if (lexer.next().kind !== 'End') throw new Error('Expected ending.');
  return type;
}

class Parser {
  private readonly lexer: Lexer;

  constructor(lexer: Lexer) {
    this.lexer = lexer;
  }

  parseExpression(): Expression {
    // Parse a function.
    if (this.tryParseGlyph(Glyph.Lambda)) {
      if (this.tryParseGlyph(Glyph.ParenLeft)) {
        const parameters = this.parseNonEmptyCommaList(() =>
          this.parseIdentifier()
        );
        this.parseGlyph(Glyph.ParenRight);
        this.parseGlyph(Glyph.Dot);
        const body = this.parseExpression();
        return parameters.reduceRight(
          (expression, parameter) =>
            Expression.function_(parameter, expression),
          body
        );
      } else {
        const parameter = this.parseIdentifier();
        this.parseGlyph(Glyph.Dot);
        const body = this.parseExpression();
        return Expression.function_(parameter, body);
      }
    }

    // Parse a binding.
    if (this.tryParseGlyph(Glyph.Let)) {
      const name = this.parseIdentifier();
      this.parseGlyph(Glyph.Equals);
      const value = this.parseExpression();
      this.parseGlyph(Glyph.In);
      const body = this.parseExpression();
      return Expression.binding(name, value, body);
    }

    // Parse an expression.
    let expression = this.tryParseUnwrappedExpression();
    if (expression === undefined) {
      throw new Error('Expected expression.');
    }

    // Parse any applications to that expression.
    while (true) {
      const argument = this.tryParseUnwrappedExpression();
      if (argument !== undefined) {
        expression = Expression.call(expression, argument);
      } else {
        break;
      }
    }

    return expression;
  }

  tryParseUnwrappedExpression(): Expression | undefined {
    // Parse an expression inside parentheses.
    if (this.tryParseGlyph(Glyph.ParenLeft)) {
      const expression = this.parseExpression();
      // Parse an annotation.
      if (this.tryParseGlyph(Glyph.Colon)) {
        const type = this.parsePolytype();
        this.parseGlyph(Glyph.ParenRight);
        return Expression.annotation(expression, type);
      } else {
        this.parseGlyph(Glyph.ParenRight);
        return expression;
      }
    }

    // Parse a boolean.
    if (this.tryParseGlyph(Glyph.True)) return Expression.boolean(true);
    if (this.tryParseGlyph(Glyph.False)) return Expression.boolean(false);

    // Parse a variable.
    const identifier = this.tryParseIdentifier();
    if (identifier !== undefined) {
      return Expression.variable(identifier);
    }

    // Parse a number.
    const number = this.tryParseNumber();
    if (number !== undefined) {
      return Expression.number(number);
    }

    return undefined;
  }

  parsePolytype(): Polytype {
    // Parse quantification.
    if (this.tryParseGlyph(Glyph.ForAll)) {
      if (this.tryParseGlyph(Glyph.ParenLeft)) {
        const bounds = this.parseNonEmptyCommaList(() => {
          // Parse a bound.
          const name = this.parseIdentifier();
          if (this.tryParseGlyph(Glyph.Equals)) {
            const type = this.parsePolytype();
            return {name, bound: Type.rigidBound(type)};
          } else {
            return {name, bound: Type.unbounded};
          }
        });
        this.parseGlyph(Glyph.ParenRight);
        this.parseGlyph(Glyph.Dot);
        const body = this.parsePolytype();
        return bounds.reduceRight(
          (type, {name, bound}) => Type.quantify(name, bound, type),
          body
        );
      } else {
        const name = this.parseIdentifier();
        this.parseGlyph(Glyph.Dot);
        const body = this.parsePolytype();
        return Type.quantifyUnbounded(name, body);
      }
    }

    return this.parseMonotype();
  }

  parseMonotype(): Monotype {
    const type = this.parseUnwrappedMonotype();

    // Parse function.
    if (this.tryParseGlyph(Glyph.Arrow)) {
      const body = this.parseMonotype();
      return Type.function_(type, body);
    }

    return type;
  }

  parseUnwrappedMonotype(): Monotype {
    // Parse a monotype inside parentheses.
    if (this.tryParseGlyph(Glyph.ParenLeft)) {
      const type = this.parseMonotype();
      this.parseGlyph(Glyph.ParenRight);
      return type;
    }

    // Parse primitives.
    if (this.tryParseGlyph(Glyph.Boolean)) return Type.boolean;
    if (this.tryParseGlyph(Glyph.Number)) return Type.number;
    if (this.tryParseGlyph(Glyph.String)) return Type.string;

    // Parse variable.
    const identifier = this.tryParseIdentifier();
    if (identifier !== undefined) {
      return Type.variable(identifier);
    }

    throw new Error('Expected type.');
  }

  parseNonEmptyCommaList<T>(parseItem: () => T): Array<T> {
    const items: Array<T> = [];

    while (true) {
      const item = parseItem();
      items.push(item);
      if (!this.tryParseGlyph(Glyph.Comma)) break;
    }

    return items;
  }

  tryParseGlyph(glyph: Glyph): boolean {
    const token = this.lexer.peek();
    if (token.kind === 'Glyph' && token.glyph === glyph) {
      this.lexer.next();
      return true;
    }
    return false;
  }

  parseGlyph(glyph: Glyph) {
    const token = this.lexer.next();
    if (token.kind !== 'Glyph' || token.glyph !== glyph) {
      throw new Error('Unexpected token.');
    }
  }

  tryParseIdentifier(): string | undefined {
    const token = this.lexer.peek();
    if (token.kind === 'Identifier') {
      this.lexer.next();
      return token.name;
    }
    return undefined;
  }

  parseIdentifier(): string {
    const token = this.lexer.next();
    if (token.kind !== 'Identifier') {
      throw new Error('Unexpected token.');
    }
    return token.name;
  }

  tryParseNumber(): number | undefined {
    const token = this.lexer.peek();
    if (token.kind === 'Number') {
      this.lexer.next();
      return token.number;
    }
    return undefined;
  }
}

type Token =
  | {readonly kind: 'Identifier'; readonly name: string}
  | {readonly kind: 'Number'; readonly number: number}
  | {readonly kind: 'Glyph'; readonly glyph: Glyph}
  | {readonly kind: 'End'};

const enum Glyph {
  Arrow = '→',
  Colon = ':',
  Comma = ',',
  Dot = '.',
  Equals = '=',
  ForAll = '∀',
  Lambda = 'λ',
  ParenLeft = '(',
  ParenRight = ')',

  // Keywords
  True = 'true',
  False = 'false',
  Let = 'let',
  In = 'in',
  Boolean = 'boolean',
  Number = 'number',
  String = 'string',
}

class Lexer {
  private readonly iterator: PeekableIterator<string>;
  private done = false;
  private peeked: Token | undefined;

  constructor(source: string) {
    this.iterator = peekable(source[Symbol.iterator]());
  }

  peek() {
    if (this.peeked === undefined) {
      this.peeked = this.next();
    }
    return this.peeked;
  }

  next(): Token {
    if (this.peeked !== undefined) {
      const token = this.peeked;
      this.peeked = undefined;
      return token;
    }

    const c = this.nextChar();

    switch (c) {
      case '→':
        return {kind: 'Glyph', glyph: Glyph.Arrow};
      case ':':
        return {kind: 'Glyph', glyph: Glyph.Colon};
      case ',':
        return {kind: 'Glyph', glyph: Glyph.Comma};
      case '.':
        return {kind: 'Glyph', glyph: Glyph.Dot};
      case '=':
        return {kind: 'Glyph', glyph: Glyph.Equals};
      case '∀':
        return {kind: 'Glyph', glyph: Glyph.ForAll};
      case 'λ':
        return {kind: 'Glyph', glyph: Glyph.Lambda};
      case '(':
        return {kind: 'Glyph', glyph: Glyph.ParenLeft};
      case ')':
        return {kind: 'Glyph', glyph: Glyph.ParenRight};

      case ' ':
      case '\n':
      case '\t':
      case '\r':
        return this.next();

      case undefined:
        return {kind: 'End'};

      default: {
        if (isIdentifierStart(c)) {
          let name = c;
          while (true) {
            const c = this.peekChar();
            if (c !== undefined && isIdentifierContinue(c)) {
              this.nextChar();
              name += c;
            } else {
              break;
            }
          }
          switch (name) {
            case 'let':
              return {kind: 'Glyph', glyph: Glyph.Let};
            case 'in':
              return {kind: 'Glyph', glyph: Glyph.In};
            case 'boolean':
              return {kind: 'Glyph', glyph: Glyph.Boolean};
            case 'number':
              return {kind: 'Glyph', glyph: Glyph.Number};
            case 'string':
              return {kind: 'Glyph', glyph: Glyph.String};
            default:
              return {kind: 'Identifier', name};
          }
        } else if (isNumber(c)) {
          let number = c;
          while (true) {
            const c = this.peekChar();
            if (c !== undefined && isNumber(c)) {
              this.nextChar();
              number += c;
            } else {
              break;
            }
          }
          if (this.peekChar() === '.') {
            this.nextChar();
            number += '.';
            while (true) {
              const c = this.peekChar();
              if (c !== undefined && isNumber(c)) {
                this.nextChar();
                number += c;
              } else {
                break;
              }
            }
          }
          const actualNumber = parseFloat(number);
          if (isNaN(actualNumber)) {
            throw new Error(`Expected number: "${number}"`);
          }
          return {kind: 'Number', number: actualNumber};
        } else {
          throw new Error(`Unexpected character: "${c}"`);
        }
      }
    }
  }

  private nextChar(): string | undefined {
    if (this.done === true) return undefined;
    const c = this.iterator.next();
    if (c.done === true) {
      this.done = true;
      return undefined;
    }
    return c.value;
  }

  private peekChar(): string | undefined {
    const step = this.iterator.peek();
    return step.done ? undefined : step.value;
  }
}

function isIdentifierStart(c: string): boolean {
  return /[_a-zA-Z]/.test(c);
}

function isIdentifierContinue(c: string): boolean {
  return /[_a-zA-Z0-9]/.test(c);
}

function isNumber(c: string): boolean {
  return /[0-9]/.test(c);
}

/**
 * A peekable iterator allows you to peek the next item without consuming it.
 * Note that peekable iterators do not allow you to pass in a value with
 * `next()`. The interface of `PeekableIterator<T>` is much more limited then the
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
