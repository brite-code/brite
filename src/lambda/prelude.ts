import * as t from '@babel/types';

import {parse} from './parse';
import {Term, abstraction, binding, native, variable} from './term';

/**
 * Parses a term with the prelude installed.
 */
export function parseWithPrelude(source: string): Term {
  let term = parse(source, [...prelude.keys()]);
  // TODO: This isn’t very efficient but will do while we don’t have a
  // module system.
  for (const [name, value] of prelude.entries()) {
    term = binding(name, value, term);
  }
  return term;
}

/**
 * Variables accessible in every program.
 */
const prelude = new Map<string, Term>();

// Identity function
prelude.set('id', parse('λx.x'));

// Constant function
prelude.set('const', parse('λx y.x'));

// Fix-point combinator (Z-combinator). We are strictly evaluated so we can’t
// use the standard fix-point combinator.
//
// https://en.wikipedia.org/wiki/Fixed-point_combinator
prelude.set('fix', parse('λf.(λx.f (λv.x x v)) (λx.f (λv.x x v))'));

// Common numbers
prelude.set('zero', native([], {js: () => t.numericLiteral(0)}));
prelude.set('one', native([], {js: () => t.numericLiteral(1)}));

// Addition
const add = native([variable(2), variable(1)], {
  js: ([x, y]) => t.binaryExpression('+', x, y),
});
prelude.set('add', abstraction('x', abstraction('y', add)));

// Subtraction
const sub = native([variable(2), variable(1)], {
  js: ([x, y]) => t.binaryExpression('-', x, y),
});
prelude.set('sub', abstraction('x', abstraction('y', sub)));

// Multiplication
const mul = native([variable(2), variable(1)], {
  js: ([x, y]) => t.binaryExpression('*', x, y),
});
prelude.set('mul', abstraction('x', abstraction('y', mul)));

// Division
const div = native([variable(2), variable(1)], {
  js: ([x, y]) => t.binaryExpression('/', x, y),
});
prelude.set('div', abstraction('x', abstraction('y', div)));

// Equality
const eq = native([variable(2), variable(1)], {
  js: ([x, y]) =>
    t.binaryExpression(
      '|',
      t.binaryExpression('===', x, y),
      t.numericLiteral(0),
    ),
});
prelude.set('eq', abstraction('x', abstraction('y', eq)));
