import * as t from '@babel/types';
import {
  Term,
  abstraction,
  native,
  application,
  getFreeVariables,
} from '../term';
import {parse} from '../parse';

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
    term = application(abstraction(name, term), value);
  }
  return term;
}

let nativeVariables: Map<string, JsTerm>;

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

  // Fix-point combinator (Z-combinator) since we are strictly evaluated.
  // https://en.wikipedia.org/wiki/Fixed-point_combinator
  variables.set('fix', parse('λf.(λx.λv.((f (x x)) v)) (λx.λv.((f (x x)) v))'));

  // Common numbers
  variables.set('zero', native([], (): t.Expression => t.numericLiteral(0)));
  variables.set('one', native([], (): t.Expression => t.numericLiteral(1)));

  // Addition
  const add: JsTerm = native(['x', 'y'], ([x, y]) =>
    t.binaryExpression('+', x, y),
  );
  variables.set('add', abstraction('x', abstraction('y', add)));

  // Subtraction
  const sub: JsTerm = native(['x', 'y'], ([x, y]) =>
    t.binaryExpression('-', x, y),
  );
  variables.set('sub', abstraction('x', abstraction('y', sub)));

  // Multiplication
  const mul: JsTerm = native(['x', 'y'], ([x, y]) =>
    t.binaryExpression('*', x, y),
  );
  variables.set('mul', abstraction('x', abstraction('y', mul)));

  // Division
  const div: JsTerm = native(['x', 'y'], ([x, y]) =>
    t.binaryExpression('/', x, y),
  );
  variables.set('div', abstraction('x', abstraction('y', div)));

  return variables;
}
