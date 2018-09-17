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
      const variable = scope.resolveVariable(term.name);
      if (variable === undefined) {
        throw new Error(`Could not resolve variable "${term.name}"`);
      }
      return variable;
    }
    case TermType.Abstraction: {
      const [{parameter, result}, statements] = scope.nest(() => {
        const parameter = scope.declareVariable(term.parameter);
        const result = serialize(term.body, scope);
        return {parameter, result};
      });
      let body;
      if (statements.length === 0) {
        body = result;
      } else {
        statements.push(t.returnStatement(result));
        body = t.blockStatement(statements);
      }
      return t.arrowFunctionExpression([parameter], body);
    }
    case TermType.Application: {
      // If we are applying an abstraction then serialize a binding to avoid a
      // function call.
      if (term.callee.type === TermType.Abstraction) {
        const init = serialize(term.argument, scope);
        const parameter = scope.declareVariable(term.callee.parameter);
        scope.addStatement(
          t.variableDeclaration('const', [
            t.variableDeclarator(parameter, init),
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
