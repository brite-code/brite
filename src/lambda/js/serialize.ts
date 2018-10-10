import * as t from '@babel/types';
import * as Immutable from 'immutable';

import {Term, TermType} from '../term';

import {ScopeStack} from './scope';

type JsTerm = Term<t.Expression>;
type StatementList = Immutable.List<t.Statement>;

/**
 * The result of serialization. Contains the expression to be returned and some
 * statements which represent side-effects on the environment.
 */
type SerializeResult = {
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
 */
function serialize(scope: ScopeStack, term: JsTerm): SerializeResult {
  switch (term.type) {
    case TermType.Variable: {
      return {
        statements: Immutable.List(),
        strict: false,
        expression: scope.resolveVariable(term.name),
      };
    }
    case TermType.Abstraction: {
      return scope.nest(() => {
        const parameter = scope.declareVariable(term.parameter);
        const {statements, expression} = serialize(scope, term.body);
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
      });
    }
    case TermType.Application: {
      // If we are applying an abstraction then serialize a binding to avoid a
      // function call.
      if (term.callee.type === TermType.Abstraction) {
        const argument = serialize(scope, term.argument);
        const parameter = scope.declareVariable(term.callee.parameter);
        const bindingStatement = t.variableDeclaration('const', [
          t.variableDeclarator(parameter, argument.expression),
        ]);
        const body = serialize(scope, term.callee.body);
        const statements = argument.statements.concat(
          bindingStatement,
          body.statements,
        ) as StatementList;
        return {
          statements,
          strict: body.strict,
          expression: body.expression,
        };
      } else {
        return combineSerializeResults(
          scope,
          [serialize(scope, term.callee), serialize(scope, term.argument)],
          ([callee, argument]) => ({
            statements: Immutable.List(),
            strict: true,
            expression: t.callExpression(callee.expression, [
              argument.expression,
            ]),
          }),
        );
      }
    }
    case TermType.Native: {
      return combineSerializeResults(
        scope,
        term.inputs.map(input => serialize(scope, input)),
        inputs => {
          let strict = false;
          const serializeInputs = inputs.map(input => {
            strict = strict || input.strict;
            return input.expression;
          });
          return {
            statements: Immutable.List(),
            strict,
            expression: term.serialize(serializeInputs),
          };
        },
      );
    }
  }
}

/**
 * Serializes a lambda calculus term to a JavaScript expression.
 */
function serializeStart(term: JsTerm): t.Program {
  const {statements, expression} = serialize(new ScopeStack(), term);
  const programStatements = statements.toArray();
  programStatements.push(t.expressionStatement(expression));
  return t.program(programStatements);
}

export {serializeStart as serialize};

/**
 * Combines an array of serialization results into a single serialization result
 * with a combiner function. Concatenates all the statement lists together and
 * maintains the ordering of strict expressions.
 */
function combineSerializeResults(
  scope: ScopeStack,
  results: ReadonlyArray<SerializeResult>,
  combine: (
    expressions: ReadonlyArray<{strict: boolean; expression: t.Expression}>,
  ) => SerializeResult,
): SerializeResult {
  // Create the initial variables we will be modifying.
  let statements = Immutable.List<t.Statement>();
  const expressions = Array(results.length);
  // Iterate through the results we were provided backwards. This way we can
  // detect if there are statements _after_ the position of a strict expression.
  // If there are then we need to hoist the expression into a statement.
  for (let i = results.length - 1; i >= 0; i--) {
    const result = results[i];
    // If our result expression is strictly evaluated and there are some
    // statements evaluated after our strict expression then we need to take
    // care to evaluate our expression in the proper order. So we hoist our
    // strict expression into a variable declaration so that it evaluates after
    // its own statements and before the other statements. We then use a
    // temporary identifier for the hoisted expression.
    if (result.strict && !statements.isEmpty()) {
      const identifier = scope.createInternalIdentifier('');
      expressions[i] = {strict: false, expression: identifier};
      const bindingStatement = t.variableDeclaration('const', [
        t.variableDeclarator(identifier, result.expression),
      ]);
      statements = result.statements.concat(
        bindingStatement,
        statements,
      ) as StatementList;
    } else {
      // Add our expression to our array and prepend our statements to the list.
      expressions[i] = {strict: result.strict, expression: result.expression};
      statements = result.statements.concat(statements) as StatementList;
    }
  }
  const result = combine(expressions);
  statements = statements.concat(result.statements) as StatementList;
  return {
    statements,
    strict: result.strict,
    expression: result.expression,
  };
}
