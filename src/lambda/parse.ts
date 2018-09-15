import {Identifier, Term, abstraction, application, variable} from './term';

const identifierStart = /\w/;
const identifierContinue = /[\w\d]/;
const whitespace = /\s/;

/**
 * Parses a lambda calculus term.
 *
 * - `x` → `variable(x)`
 * - `(λx.y)` → `abstraction(x, y)`
 * - `(x y)` → `application(x, y)`
 */
export function parse(source: string): Term {
  const [term, lastStep] = parseTerm(source[Symbol.iterator]());
  if (!lastStep.done) {
    throw new Error(`Unexpected token "${lastStep.value}" expected ending`);
  }
  return term;
}

/**
 * Does the actual lambda calculus parsing on an iterator.
 */
function parseTerm(iterator: Iterator<string>): [Term, IteratorResult<string>] {
  const step = parseToken(iterator);

  // Parse an abstraction.
  if (step.value === 'λ') {
    const [parameter, nextStep] = parseIdentifier(iterator);
    expectToken(nextStep, '.');
    const [body, nextNextStep] = parseTerm(iterator);
    return [abstraction(parameter, body), nextNextStep];
  }

  // Parses an unwrapped term.
  let [term, nextStep] = parseUnwrappedTerm(iterator, step);

  // Parse an application.
  while (!nextStep.done && nextStep.value !== ')') {
    const [argument, nextNextStep] = parseUnwrappedTerm(iterator, nextStep);
    term = application(term, argument);
    nextStep = nextNextStep;
  }

  return [term, nextStep];
}

/**
 * Parses an unwrapped term. Either a variable or a wrapped term.
 */
function parseUnwrappedTerm(
  iterator: Iterator<string>,
  step: IteratorResult<string> = parseToken(iterator),
): [Term, IteratorResult<string>] {
  // Parse a term inside parentheses.
  if (step.value === '(') {
    const [term, nextStep] = parseTerm(iterator);
    expectToken(nextStep, ')');
    return [term, parseWhitespace(iterator)];
  }

  // Parse a variable.
  if (identifierStart.test(step.value)) {
    const [identifier, nextToken] = parseIdentifier(iterator, step);
    return [variable(identifier), nextToken];
  }

  // Throw an unexpected token error.
  const token = step.value;
  throw new Error(`Unexpected token "${token}" expected lambda calculus term`);
}

/**
 * Parses an identifier from the iterator.
 *
 * Provide a second parameter when you needed to peek to determine whether or
 * not to parse an identifier.
 */
function parseIdentifier(
  iterator: Iterator<string>,
  step: IteratorResult<string> = parseToken(iterator),
): [Identifier, IteratorResult<string>] {
  if (!identifierStart.test(step.value)) {
    throw new Error(`Unexpected token "${step.value}" expected identifier`);
  }
  let identifier = step.value;
  step = iterator.next();
  while (!step.done && identifierContinue.test(step.value)) {
    identifier += step.value;
    step = iterator.next();
  }
  if (!step.done && whitespace.test(step.value)) {
    step = parseWhitespace(iterator);
  }
  return [identifier, step];
}

/**
 * Parses all the whitespace in the iterator and returns the next step.
 */
function parseWhitespace(iterator: Iterator<string>): IteratorResult<string> {
  let step = iterator.next();
  while (!step.done && whitespace.test(step.value)) {
    step = iterator.next();
  }
  return step;
}

/**
 * Returns the next "token" in the iterator. Throws if the iterator ends.
 */
function parseToken(iterator: Iterator<string>): IteratorResult<string> {
  const step = parseWhitespace(iterator);
  if (step.done) throw new Error('Unexpected ending');
  return step;
}

/**
 * Throws an error if the token is not what was expected.
 */
function expectToken(step: IteratorResult<string>, token: string) {
  if (step.done) throw new Error('Unexpected ending');
  if (step.value !== token) {
    throw new Error(
      `Unexpected token: "${step.value}" expected token "${token}"`,
    );
  }
}
