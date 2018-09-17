import * as t from '@babel/types';
import {Term, TermType} from '../term';
import {ScopeStack} from './scope';

type JsTerm = Term<t.Expression>;

/**
 * The result of serialization. Contains the expression to be returned and some
 * statements which represent side-effects on the environment.
 */
type Serialization = {
  /**
   * Side-effects in the environment.
   */
  readonly statements: RecursiveArray<t.Statement>;
  /**
   * Should the expression be evaluated strictly? If false then the expression
   * may be ordered anywhere in our statements.
   */
  readonly strict: boolean;
  /**
   * The expression which serialization evaluates to.
   */
  readonly expression: t.Expression;
};

/**
 * Serializes a lambda calculus term to a JavaScript expression in the provided
 * scope stack.
 *
 * Returns a function which builds the resulting JavaScript expression. The
 * function should be called precisely at the place where it should be executed.
 * This way calling `serialize()` adds statements then calling the result of
 * `serialize` adds the resulting expression.
 */
function serialize(term: JsTerm, scope: ScopeStack): Serialization {
  switch (term.type) {
    case TermType.Variable: {
      return {
        statements: [],
        strict: false,
        expression: scope.resolveVariable(term.name),
      };
    }
    case TermType.Abstraction: {
      return scope.nest(() => {
        const parameter = scope.declareVariable(term.parameter);
        const result = serialize(term.body, scope);
        const statements = flatten(result.statements);
        let body: t.Expression | t.BlockStatement;
        if (statements.length === 0) {
          body = result.expression;
        } else {
          statements.push(t.returnStatement(result.expression));
          body = t.blockStatement(statements);
        }
        return {
          statements: [],
          strict: false,
          expression: t.arrowFunctionExpression([parameter], body),
        };
      });
    }
    case TermType.Application: {
      // If we are applying an abstraction then serialize a binding to avoid a
      // function call.
      if (term.callee.type === TermType.Abstraction) {
        const argument = serialize(term.argument, scope);
        const parameter = scope.declareVariable(term.callee.parameter);
        argument.statements.push(
          t.variableDeclaration('const', [
            t.variableDeclarator(parameter, argument.expression),
          ]),
        );
        const body = serialize(term.callee.body, scope);
        return {
          statements: concat(argument.statements, body.statements),
          strict: body.strict,
          expression: body.expression,
        };
      } else {
        // If the callee is strictly evaluated and the argument has some
        // statements then we need to make sure our callee expression executes
        // before our argument statements. So add a variable declaration with
        // the callee expression.
        const callee = serialize(term.callee, scope);
        const argument = serialize(term.argument, scope);
        const calleeStatements = callee.statements;
        let calleeExpression;
        if (callee.strict && argument.statements.length > 0) {
          const identifier = scope.createInternalIdentifier('');
          calleeExpression = identifier;
          calleeStatements.push(
            t.variableDeclaration('const', [
              t.variableDeclarator(identifier, callee.expression),
            ]),
          );
        } else {
          calleeExpression = callee.expression;
        }
        return {
          statements: concat(calleeStatements, argument.statements),
          strict: true,
          expression: t.callExpression(calleeExpression, [argument.expression]),
        };
      }
    }
    case TermType.Native: {
      return {
        statements: [],
        strict: false,
        expression: term.serialize(
          term.variables.map(name => scope.resolveVariable(name)),
        ),
      };
    }
  }
}

/**
 * Serializes a lambda calculus term to a JavaScript expression.
 */
function serializeStart(term: JsTerm): t.Program {
  const result = serialize(term, new ScopeStack());
  const statements = flatten(result.statements);
  statements.push(t.expressionStatement(result.expression));
  return t.program(statements);
}

export {serializeStart as serialize};

interface RecursiveArray<T> extends Array<T | RecursiveArray<T>> {}

function concat<T>(
  left: RecursiveArray<T>,
  right: RecursiveArray<T>,
): RecursiveArray<T> {
  if (left.length === 0) return right;
  if (right.length === 0) return left;
  return [left, right];
}

function flatten<T>(array: RecursiveArray<T>): Array<T> {
  const newArray: Array<T> = [];
  flattenInto(array, newArray);
  return newArray;
}

function flattenInto<T>(array: RecursiveArray<T>, newArray: Array<T>) {
  for (let i = 0; i < array.length; i++) {
    const item = array[i];
    if (Array.isArray(item)) {
      flattenInto(item, newArray);
    } else {
      newArray.push(item);
    }
  }
}
