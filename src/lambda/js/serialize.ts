import * as t from '@babel/types';
import * as Immutable from 'immutable';

import {ConditionalTerm, Term, TermType} from '../term';

import {Scope} from './scope';

/**
 * The result of serialization. Contains the expression to be returned and some
 * statements which represent side-effects on the environment.
 */
type SerializedTerm = {
  /**
   * Side-effects in the environment. We use an immutable list since we want to
   * efficiently concatenate lists together pretty often.
   */
  readonly statements: Immutable.Iterable<number, t.Statement>;
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
          const {statements, expression} = serializeTermWithStatementOutput(
            scope,
            term.body,
            expression => t.returnStatement(expression),
          );
          let body: t.Expression | t.BlockStatement;
          if (expression !== undefined) {
            if (statements.isEmpty()) {
              body = expression;
            } else {
              body = t.blockStatement(
                statements.concat(t.returnStatement(expression)).toArray(),
              );
            }
          } else {
            body = t.blockStatement(statements.toArray());
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
          );
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
          );
          return {
            statements,
            strict: true,
            expression: t.callExpression(variable, [argument.expression]),
          };
        } else {
          return {
            statements: callee.statements.concat(argument.statements),
            strict: true,
            expression: t.callExpression(callee.expression, [
              argument.expression,
            ]),
          };
        }
      }
    }

    // If a conditional is being serialized and we don’t have a custom statement
    // output for if-statements then we might need a phi variable.
    case TermType.Conditional: {
      const phi = scope.newVariable('$phi');
      const out = (expression: t.Expression) =>
        t.expressionStatement(t.assignmentExpression('=', phi, expression));
      const test = serializeTerm(scope, term.test);
      const left = scope.block(() =>
        serializeTermWithStatementOutput(scope, term.consequent, out),
      );
      const right = scope.block(() =>
        serializeTermWithStatementOutput(scope, term.alternate, out),
      );
      if (
        !left.statements.isEmpty() ||
        !right.statements.isEmpty() ||
        left.expression === undefined ||
        right.expression === undefined
      ) {
        const statements = test.statements.concat(
          t.variableDeclaration('let', [t.variableDeclarator(phi)]),
          t.ifStatement(
            test.expression,
            t.blockStatement(
              (left.expression === undefined
                ? left.statements
                : left.statements.concat(out(left.expression))
              ).toArray(),
            ),
            t.blockStatement(
              (right.expression === undefined
                ? right.statements
                : right.statements.concat(out(right.expression))
              ).toArray(),
            ),
          ),
        );
        return {
          statements,
          strict: false,
          expression: phi,
        };
      } else {
        return {
          statements: test.statements,
          strict: test.strict || left.strict || right.strict,
          expression: t.conditionalExpression(
            test.expression,
            left.expression,
            right.expression,
          ),
        };
      }
    }

    // Serialize native terms by serializing all the inputs of the term and then
    // calling the JavaScript serializer function for the native term.
    case TermType.Native: {
      return combine(
        scope,
        term.inputs.map(input => serializeTerm(scope, input)),
        inputs => {
          let strict = false;
          const serializeInputs = inputs.map(input => {
            strict = strict || input.strict;
            return input.expression;
          });
          return {
            statements: Immutable.List(),
            strict,
            expression: term.serializers.js(serializeInputs),
          };
        },
      );
    }
  }
}

/**
 * Serializes a term with a custom output function for returning expressions
 * from the statement. For instance, perhaps the last expression should
 * be returned.
 */
function serializeTermWithStatementOutput(
  scope: Scope,
  term: Term,
  out: (expression: t.Expression) => t.Statement,
): {
  readonly statements: Immutable.Iterable<number, t.Statement>;
  readonly strict: boolean;
  readonly expression: t.Expression | undefined;
} {
  // Special case bindings. The body of a binding should be serialized with our
  // custom statement output function.
  if (
    term.type === TermType.Application &&
    term.callee.type === TermType.Abstraction
  ) {
    const callee = term.callee;
    const argument = serializeTerm(scope, term.argument);
    return scope.binding(callee.parameter, parameter => {
      const bindingStatement = t.variableDeclaration('const', [
        t.variableDeclarator(parameter, argument.expression),
      ]);
      const body = serializeTermWithStatementOutput(scope, callee.body, out);
      const statements = argument.statements.concat(
        bindingStatement,
        body.statements,
      );
      return {
        statements,
        strict: body.strict,
        expression: body.expression,
      };
    });
  }

  // Special case conditionals so that the last statement in each conditional
  // branch may use the statement output function.
  if (term.type === TermType.Conditional) {
    const test = serializeTerm(scope, term.test);
    const left = scope.block(() =>
      serializeTermWithStatementOutput(scope, term.consequent, out),
    );
    const right = scope.block(() =>
      serializeTermWithStatementOutput(scope, term.alternate, out),
    );
    if (
      !left.statements.isEmpty() ||
      !right.statements.isEmpty() ||
      left.expression === undefined ||
      right.expression === undefined
    ) {
      const statements = test.statements.concat(
        t.ifStatement(
          test.expression,
          t.blockStatement(
            (left.expression === undefined
              ? left.statements
              : left.statements.concat(out(left.expression))
            ).toArray(),
          ),
          t.blockStatement(
            (right.expression === undefined
              ? right.statements
              : right.statements.concat(out(right.expression))
            ).toArray(),
          ),
        ),
      );
      return {
        statements,
        strict: false,
        expression: undefined,
      };
    } else {
      return {
        statements: test.statements,
        strict: test.strict || left.strict || right.strict,
        expression: t.conditionalExpression(
          test.expression,
          left.expression,
          right.expression,
        ),
      };
    }
  }

  const {statements, strict, expression} = serializeTerm(scope, term);
  return {statements, strict, expression};
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

/**
 * Combines an array of serialized terms into a single serialized term with a
 * combiner function. Concatenates all the statement lists together and
 * maintains the ordering of strict expressions.
 */
function combine(
  scope: Scope,
  terms: ReadonlyArray<SerializedTerm>,
  combiner: (
    expressions: ReadonlyArray<{strict: boolean; expression: t.Expression}>,
  ) => SerializedTerm,
): SerializedTerm {
  // Create the initial variables we will be modifying.
  let statements: Immutable.Iterable<number, t.Statement> = Immutable.List();
  const expressions = Array(terms.length);
  // Iterate through the results we were provided backwards. This way we can
  // detect if there are statements _after_ the position of a strict expression.
  // If there are then we need to hoist the expression into a statement.
  for (let i = terms.length - 1; i >= 0; i--) {
    const result = terms[i];
    // If our result expression is strictly evaluated and there are some
    // statements evaluated after our strict expression then we need to take
    // care to evaluate our expression in the proper order. So we hoist our
    // strict expression into a variable declaration so that it evaluates after
    // its own statements and before the other statements. We then use a
    // temporary identifier for the hoisted expression.
    if (result.strict && !statements.isEmpty()) {
      const identifier = scope.newInternalVariable();
      expressions[i] = {strict: false, expression: identifier};
      const bindingStatement = t.variableDeclaration('const', [
        t.variableDeclarator(identifier, result.expression),
      ]);
      statements = result.statements.concat(bindingStatement, statements);
    } else {
      // Add our expression to our array and prepend our statements to the list.
      expressions[i] = {strict: result.strict, expression: result.expression};
      statements = result.statements.concat(statements);
    }
  }
  const result = combiner(expressions);
  statements = statements.concat(result.statements);
  return {
    statements,
    strict: result.strict,
    expression: result.expression,
  };
}
