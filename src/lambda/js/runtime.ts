import * as t from '@babel/types';

import {parse} from '../parse';
import {Term, abstraction, native, variable} from '../term';

type JsTerm = Term<t.Expression>;

/**
 * Adds the applicable runtime to some lambda calculus term.
 */
export function addRuntime(initialTerm: JsTerm): JsTerm {
  const free = getFreeVariables(initialTerm);
  const nativeVariables = getNativeVariables();
  let term = initialTerm;
  for (const [name, value] of nativeVariables) {
    if (!free.has(name)) continue;
    // TODO: This isn’t very efficient. Preferably we would provide our native
    // variables directly to a compiler which would pull them in on demand.
    term = binding(name, value, term);
  }
  return term;
}

let nativeVariables: Map<string, JsTerm> | undefined;

/**
 * Gets the native variables for our programs.
 *
 * If this function has not been called before we initialize the
 * native variables.
 */
function getNativeVariables(): Map<string, JsTerm> {
  if (nativeVariables === undefined) {
    nativeVariables = initNativeVariables();
  }
  return nativeVariables;
}

/**
 * Initializes the native variables for our programs.
 */
function initNativeVariables(): Map<string, JsTerm> {
  const variables = new Map<string, JsTerm>();

  // Identity function
  variables.set('id', parse('λx.x'));

  // Constant function
  variables.set('const', parse('λx.λy.x'));

  // Fix-point combinator (Z-combinator). We are strictly evaluated so we can’t
  // use the standard fix-point combinator.
  //
  // https://en.wikipedia.org/wiki/Fixed-point_combinator
  variables.set('fix', parse('λf.(λx.f (λv.x x v)) (λx.f (λv.x x v))'));

  // Common numbers
  variables.set('zero', native([], (): t.Expression => t.numericLiteral(0)));
  variables.set('one', native([], (): t.Expression => t.numericLiteral(1)));

  // Addition
  const add: JsTerm = native([variable('x'), variable('y')], ([x, y]) =>
    t.binaryExpression('+', x, y),
  );
  variables.set('add', abstraction('x', abstraction('y', add)));

  // Subtraction
  const sub: JsTerm = native([variable('x'), variable('y')], ([x, y]) =>
    t.binaryExpression('-', x, y),
  );
  variables.set('sub', abstraction('x', abstraction('y', sub)));

  // Multiplication
  const mul: JsTerm = native([variable('x'), variable('y')], ([x, y]) =>
    t.binaryExpression('*', x, y),
  );
  variables.set('mul', abstraction('x', abstraction('y', mul)));

  // Division
  const div: JsTerm = native([variable('x'), variable('y')], ([x, y]) =>
    t.binaryExpression('/', x, y),
  );
  variables.set('div', abstraction('x', abstraction('y', div)));

  // Conditional
  const if_: JsTerm = native(
    [variable('x'), variable('y'), variable('z')],
    ([x, y, z]) =>
      t.conditionalExpression(
        x,
        t.callExpression(y, []),
        t.callExpression(z, []),
      ),
  );
  variables.set(
    'if',
    abstraction('x', abstraction('y', abstraction('z', if_))),
  );

  // Equality
  const eq: JsTerm = native([variable('x'), variable('y')], ([x, y]) =>
    t.binaryExpression('===', x, y),
  );
  variables.set('eq', abstraction('x', abstraction('y', eq)));

  return variables;
}
