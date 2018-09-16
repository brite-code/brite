import * as t from '@babel/types';
import {Term, TermType} from './term';

/**
 * Compiles a lambda calculus term to a JavaScript expression.
 */
export function serializeJs(term: Term<t.Expression>): t.Expression {
  switch (term.type) {
    case TermType.Variable: {
      return t.identifier(term.name);
    }
    case TermType.Abstraction: {
      return t.arrowFunctionExpression(
        [t.identifier(term.parameter)],
        serializeJs(term.body),
      );
    }
    case TermType.Application: {
      return t.callExpression(serializeJs(term.callee), [
        serializeJs(term.argument),
      ]);
    }
    case TermType.Native: {
      return term.serialize(term.variables.map(name => t.identifier(name)));
    }
  }
}
