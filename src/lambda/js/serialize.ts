import * as t from '@babel/types';
import * as Immutable from 'immutable';

import {Term, TermType} from '../term';

import {Scope} from './scope';

type StatementList = Immutable.List<t.Statement>;

/**
 * The result of serialization. Contains the expression to be returned and some
 * statements which represent side-effects on the environment.
 */
type SerializedTerm = {
  /**
   * Side-effects in the environment. We use an immutable list since we want to
   * efficiently concatenate lists together pretty often.
   */
  readonly statements: StatementList;
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
function serializeTerm(scope: Scope, term: Term): SerializedTerm {
  switch (term.type) {
    case TermType.Variable: {
      return {
        statements: Immutable.List(),
        strict: false,
        expression: scope.resolve(term.index),
      };
    }
    case TermType.Abstraction: {
      return scope.block(() =>
        scope.binding(term.parameter, parameter => {
          const {statements, expression} = serializeTerm(scope, term.body);
          let body;
          if (statements.isEmpty()) {
            body = expression;
          } else {
            const blockStatements = statements.toArray();
            blockStatements.push(t.returnStatement(expression));
            body = t.blockStatement(blockStatements);
          }
          return {
            statements: Immutable.List<t.Statement>(),
            strict: false,
            expression: t.arrowFunctionExpression([parameter], body),
          };
        }),
      );
    }
    case TermType.Application: {
      // If we are applying an abstraction then serialize a binding to avoid a
      // function call.
      if (term.callee.type === TermType.Abstraction) {
        const callee = term.callee;
        const argument = serializeTerm(scope, term.argument);
        return scope.binding(callee.parameter, parameter => {
          const bindingStatement = t.variableDeclaration('const', [
            t.variableDeclarator(parameter, argument.expression),
          ]);
          const body = serializeTerm(scope, callee.body);
          const statements = argument.statements.concat(
            bindingStatement,
            body.statements,
          ) as StatementList;
          return {
            statements,
            strict: body.strict,
            expression: body.expression,
          };
        });
      } else {
        const callee = serializeTerm(scope, term.callee);
        const argument = serializeTerm(scope, term.argument);
        // If the callee needs to be evaluated strictly and the argument adds
        // some statements, then we need to hoist the callee expression above
        // the argument statements.
        if (callee.strict && !argument.statements.isEmpty()) {
          const variable = scope.newInternalVariable();
          const statement = t.variableDeclaration('const', [
            t.variableDeclarator(variable, callee.expression),
          ]);
          const statements = callee.statements.concat(
            statement,
            argument.statements,
          ) as StatementList;
          return {
            statements,
            strict: true,
            expression: t.callExpression(variable, [argument.expression]),
          };
        } else {
          return {
            statements: callee.statements.concat(
              argument.statements,
            ) as StatementList,
            strict: true,
            expression: t.callExpression(callee.expression, [
              argument.expression,
            ]),
          };
        }
      }
    }
  }
}

/**
 * Serializes a lambda calculus term to a JavaScript program.
 */
export function serialize(term: Term): t.Program {
  const {statements, expression} = serializeTerm(new Scope(), term);
  const programStatements = statements.toArray();
  programStatements.push(t.expressionStatement(expression));
  return t.program(programStatements);
}
