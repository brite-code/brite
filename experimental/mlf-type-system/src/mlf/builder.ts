import {Reported} from './diagnostics';
import {Expression} from './expression';
import {Bound, MonomorphicType, PolymorphicType, Type} from './type';

export function variableExpression(identifier: string): Expression {
  return {
    type: undefined,
    description: {kind: 'Variable', identifier},
  };
}

export function variableExpressionTyped(
  type: Type,
  identifier: string
): Expression<never, Type> {
  return {
    type,
    description: {kind: 'Variable', identifier},
  };
}

export function booleanExpression(value: boolean): Expression {
  return {
    type: undefined,
    description: {kind: 'Constant', constant: {kind: 'Boolean', value}},
  };
}

export function booleanExpressionTyped(
  value: boolean
): Expression<never, Type> {
  return {
    type: booleanType,
    description: {kind: 'Constant', constant: {kind: 'Boolean', value}},
  };
}

export function numberExpression(value: number): Expression {
  return {
    type: undefined,
    description: {kind: 'Constant', constant: {kind: 'Number', value}},
  };
}

export function numberExpressionTyped(value: number): Expression<never, Type> {
  return {
    type: numberType,
    description: {kind: 'Constant', constant: {kind: 'Number', value}},
  };
}

export function stringExpression(value: string): Expression {
  return {
    type: undefined,
    description: {kind: 'Constant', constant: {kind: 'String', value}},
  };
}

export function stringExpressionTyped(value: string): Expression<never, Type> {
  return {
    type: stringType,
    description: {kind: 'Constant', constant: {kind: 'String', value}},
  };
}

export function functionExpression(
  parameter: string,
  body: Expression
): Expression {
  return {
    type: undefined,
    description: {kind: 'Function', parameter, body},
  };
}

export function functionExpressionTyped<Diagnostic = never>(
  type: Type,
  parameter: string,
  body: Expression<Diagnostic, Type>
): Expression<Diagnostic, Type> {
  return {
    type,
    description: {kind: 'Function', parameter, body},
  };
}

export function callExpression(
  callee: Expression,
  argument: Expression
): Expression {
  return {
    type: undefined,
    description: {kind: 'Call', callee, argument},
  };
}

export function callExpressionTyped<Diagnostic = never>(
  type: Type,
  callee: Expression<Diagnostic, Type>,
  argument: Expression<Diagnostic, Type>
): Expression<Diagnostic, Type> {
  return {
    type,
    description: {kind: 'Call', callee, argument},
  };
}

export function bindingExpression(
  binding: string,
  value: Expression,
  body: Expression
): Expression {
  return {
    type: undefined,
    description: {kind: 'Binding', binding, value, body},
  };
}

export function bindingExpressionTyped<Diagnostic = never>(
  binding: string,
  value: Expression<Diagnostic, Type>,
  body: Expression<Diagnostic, Type>
): Expression<Diagnostic, Type> {
  return {
    type: body.type,
    description: {kind: 'Binding', binding, value, body},
  };
}

export function errorExpression<Diagnostic>(
  error: Reported<Diagnostic>
): Expression<Diagnostic> {
  return {
    type: undefined,
    description: {kind: 'Error', error},
  };
}

export function errorExpressionTyped<Diagnostic>(
  type: Type,
  error: Reported<Diagnostic>
): Expression<Diagnostic, Type> {
  return {
    type,
    description: {kind: 'Error', error},
  };
}

export function variableType(identifier: string): MonomorphicType {
  return {kind: 'Variable', identifier};
}

export const booleanType: MonomorphicType = {
  kind: 'Constant',
  constant: {kind: 'Boolean'},
};

export const numberType: MonomorphicType = {
  kind: 'Constant',
  constant: {kind: 'Number'},
};

export const stringType: MonomorphicType = {
  kind: 'Constant',
  constant: {kind: 'String'},
};

export function functionType(
  parameter: MonomorphicType,
  body: MonomorphicType
): MonomorphicType {
  return {kind: 'Function', parameter, body};
}

export function quantifiedType(
  binding: string,
  bound: Bound,
  body: PolymorphicType
): PolymorphicType {
  return {kind: 'Quantified', binding, bound, body};
}

export const bottomType: PolymorphicType = {kind: 'Bottom'};

export function rigidBound(type: PolymorphicType): Bound {
  return {kind: 'rigid', type};
}

export function flexibleBound(type: PolymorphicType): Bound {
  return {kind: 'flexible', type};
}
