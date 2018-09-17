import * as t from '@babel/types';
import {Term, TermType} from '../term';
import {ScopeStack} from './scope';

type JsTerm = Term<t.Expression>;

/**
 * Serializes a lambda calculus term to a JavaScript expression in the provided
 * scope stack.
 */
function serialize(term: JsTerm, scope: ScopeStack): t.Expression {
  switch (term.type) {
    case TermType.Variable: {
      return t.identifier(term.name);
    }
    case TermType.Abstraction: {
      const [result, statements] = scope.nest(() =>
        serialize(term.body, scope),
      );
      let body;
      if (statements.length === 0) {
        body = result;
      } else {
        statements.push(t.returnStatement(result));
        body = t.blockStatement(statements);
      }
      return t.arrowFunctionExpression([t.identifier(term.parameter)], body);
    }
    case TermType.Application: {
      // If we are applying an abstraction then serialize a binding to avoid a
      // function call.
      if (term.callee.type === TermType.Abstraction) {
        scope.addStatement(
          t.variableDeclaration('const', [
            t.variableDeclarator(
              t.identifier(term.callee.parameter),
              serialize(term.argument, scope),
            ),
          ]),
        );
        return serialize(term.callee.body, scope);
      } else {
        return t.callExpression(serialize(term.callee, scope), [
          serialize(term.argument, scope),
        ]);
      }
    }
    case TermType.Native: {
      return term.serialize(term.variables.map(name => t.identifier(name)));
    }
  }
}

/**
 * Serializes a lambda calculus term to a JavaScript expression.
 */
function serializeStart(term: JsTerm): t.Program {
  const [result, statements] = ScopeStack.create(scope =>
    serialize(term, scope),
  );
  statements.push(t.expressionStatement(result));
  return t.program(statements);
}

export {serializeStart as serialize};
