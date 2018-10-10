import * as Immutable from 'immutable';

import {
  Identifier,
  Term,
  abstraction,
  application,
  binding,
  variable,
} from './term';

/**
 * Parses a lambda calculus term.
 *
 * - `x` → `variable(x)`
 * - `(λx.y)` → `abstraction(x, y)`
 * - `(x y)` → `application(x, y)`
 * - `let x = y in z` → `binding(x, y, z)`
 */
export function parse<T>(source: string): Term<T> {
  const iterator = peekable(tokenize(source[Symbol.iterator]()));
  const term = parseTerm<T>(iterator, Scope.empty());
  const step = iterator.next();
  if (!step.done) {
    throw new Error(`Unexpected token "${step.value.type}" expected ending`);
  }
  return term;
}

/**
 * Does the actual lambda calculus parsing on an iterator.
 */
function parseTerm<T>(
  iterator: PeekableIterator<Token>,
  scope: Scope,
): Term<T> {
  // Parse an abstraction.
  if (tryParseToken(iterator, TokenType.Lambda)) {
    const [newScope, name] = Scope.declare(scope, parseIdentifier(iterator));
    parseToken(iterator, TokenType.Dot);
    const body = parseTerm<T>(iterator, newScope);
    return abstraction(name, body);
  }

  // Parse a binding
  if (tryParseToken(iterator, TokenType.Let)) {
    const [newScope, name] = Scope.declare(scope, parseIdentifier(iterator));
    parseToken(iterator, TokenType.Equals);
    const value = parseTerm<T>(iterator, scope);
    parseToken(iterator, TokenType.In);
    const body = parseTerm<T>(iterator, newScope);
    return binding<T>(name, value, body);
  }

  // Parses an unwrapped term.
  let term = parseUnwrappedTerm<T>(iterator, scope);
  if (term === undefined) {
    const step = iterator.next();
    if (step.done) throw new Error('Unexpected ending expected term');
    throw new Error(`Unexpected token "${step.value.type}" expected term`);
  }

  // Parse as many application arguments as we can.
  while (true) {
    const argument = parseUnwrappedTerm<T>(iterator, scope);
    if (argument === undefined) {
      break;
    } else {
      term = application(term, argument);
    }
  }

  return term;
}

/**
 * Parses an unwrapped term. Either a variable or a wrapped term. Returns
 * undefined if a term could not be parsed.
 */
function parseUnwrappedTerm<T>(
  iterator: PeekableIterator<Token>,
  scope: Scope,
): Term<T> | undefined {
  // Parse a term inside parentheses.
  if (tryParseToken(iterator, TokenType.ParenLeft)) {
    const term = parseTerm<T>(iterator, scope);
    parseToken(iterator, TokenType.ParenRight);
    return term;
  }

  // Parse a variable.
  const step = iterator.peek();
  if (!step.done && step.value.type === TokenType.Identifier) {
    iterator.next();
    return variable(Scope.resolve(scope, step.value.data));
  }

  // Otherwise we couldn’t parse any unwrapped terms.
  return undefined;
}

/**
 * Parses a token in the iterator and throws if it was not found.
 */
function parseToken(iterator: PeekableIterator<Token>, token: TokenType) {
  const step = iterator.next();
  if (step.done) {
    throw new Error(`Unexpected ending expected "${token}"`);
  }
  if (step.value.type !== token) {
    throw new Error(
      `Unexpected token "${step.value.type}" expected token "${token}"`,
    );
  }
}

/**
 * Tries to parse a token of the provided type. Returns true if it was found and
 * false if it was not.
 */
function tryParseToken(
  iterator: PeekableIterator<Token>,
  token: TokenType,
): boolean {
  const step = iterator.peek();
  const found = !step.done && step.value.type === token;
  if (found) iterator.next();
  return found;
}

/**
 * Parses an identifier from the iterator. Throws if no identifier was found.
 */
function parseIdentifier(iterator: PeekableIterator<Token>): string {
  const step = iterator.next();
  if (step.done) {
    throw new Error('Unexpected ending expected identifier');
  }
  if (step.value.type !== TokenType.Identifier) {
    throw new Error(
      `Unexpected token "${step.value.type}" expected identifier`,
    );
  }
  return step.value.data;
}

type Scope = {
  readonly identifiers: Array<string>;
  readonly variables: Immutable.Map<string, Identifier>;
};

namespace Scope {
  export function empty(): Scope {
    return {identifiers: [], variables: Immutable.Map()};
  }

  export function declare(scope: Scope, name: string): [Scope, Identifier] {
    const identifier = scope.identifiers.length;
    scope.identifiers.push(name);
    const newScope = {
      identifiers: scope.identifiers,
      variables: scope.variables.set(name, identifier),
    };
    return [newScope, identifier];
  }

  export function resolve<T>(scope: Scope, name: string): Identifier {
    const identifier = scope.variables.get(name);
    if (identifier === undefined) {
      throw new Error(`Variable "${name}" was not declared.`);
    }
    return identifier;
  }
}

/**
 * The type of a token.
 */
const enum TokenType {
  Lambda = 'λ',
  Dot = '.',
  ParenLeft = '(',
  ParenRight = ')',
  Equals = '=',
  Let = 'let',
  In = 'in',
  Identifier = 'identifier',
}

/**
 * A token value. Mostly the same as `TokenType` except that some types are
 * associated with a value. Like `TokenType.Identifier`.
 */
type Token =
  | {type: TokenType.Lambda}
  | {type: TokenType.Dot}
  | {type: TokenType.ParenLeft}
  | {type: TokenType.ParenRight}
  | {type: TokenType.Equals}
  | {type: TokenType.Let}
  | {type: TokenType.In}
  | {type: TokenType.Identifier; data: string};

const identifierStart = /\w/;
const identifierContinue = /[\w\d]/;
const whitespace = /\s/;

/**
 * Takes a source iterator of characters and returns an iterator of tokens. The
 * token iterator is much easier to work with.
 */
function* tokenize(source: Iterator<string>): IterableIterator<Token> {
  let step = source.next();
  let identifier: string | undefined = undefined;
  while (true) {
    // If an identifier has been started either add to the identifier or yield
    // the completed identifier or keyword.
    if (identifier !== undefined) {
      if (!step.done && identifierContinue.test(step.value)) {
        identifier += step.value;
        step = source.next();
        continue;
      } else {
        if (identifier === 'let') {
          yield {type: TokenType.Let};
        } else if (identifier === 'in') {
          yield {type: TokenType.In};
        } else {
          yield {type: TokenType.Identifier, data: identifier};
        }
        identifier = undefined;
      }
    }
    // If we are done then break out of the loop!
    if (step.done) {
      break;
    }
    if (step.value === 'λ') {
      yield {type: TokenType.Lambda};
    } else if (step.value === '.') {
      yield {type: TokenType.Dot};
    } else if (step.value === '(') {
      yield {type: TokenType.ParenLeft};
    } else if (step.value === ')') {
      yield {type: TokenType.ParenRight};
    } else if (step.value === '=') {
      yield {type: TokenType.Equals};
    } else if (identifierStart.test(step.value)) {
      // Start an identifier with this step.
      identifier = step.value;
    } else if (whitespace.test(step.value)) {
      // noop
    } else {
      throw new Error(`Unexpected character "${step.value}"`);
    }
    step = source.next();
  }
}

/**
 * A peekable iterator allows you to peek the next item without consuming it.
 * Note that peekable iterators do not allow you to pass in a value with
 * `next()`. The interface of `PeekableIterator<T>` is much more limited then
 * the interface of `Iterator<T>` to allow for peeking.
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
