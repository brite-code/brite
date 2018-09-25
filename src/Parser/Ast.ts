import {List1, List2} from '../Utils/ListN';

import {Identifier} from './Identifier';
import {Loc} from './Loc';

export const enum Access {
  Public = 'Public',
  Private = 'Private',
  Protected = 'Protected',
}

/**
 * Any node in our AST.
 */
export interface Node {
  readonly type: unknown;
  readonly loc: Loc;
}

export type Name = {
  readonly loc: Loc;
  readonly identifier: Identifier;
};

export type Declaration =
  | TypeDeclaration
  | FunctionDeclaration
  | ClassDeclaration
  | InterfaceDeclaration;

export const enum DeclarationType {
  Type,
  Function,
  Class,
  Interface,
}

export interface NamedDeclaration extends Node {
  readonly access: Access;
  readonly name: Name;
}

export interface TypeDeclaration extends NamedDeclaration {
  readonly type: DeclarationType.Type;
  readonly typeParameters: ReadonlyArray<TypeParameter>;
  readonly value: Type;
}

export interface FunctionDeclaration extends NamedDeclaration {
  readonly type: DeclarationType.Function;
  readonly typeParameters: ReadonlyArray<TypeParameter>;
  readonly parameters: ReadonlyArray<FunctionParameter>;
  readonly return: Type | undefined;
  readonly body: Expression;
}

export type FunctionParameter = {
  readonly binding: Pattern;
  readonly annotation: Type | undefined;
};

/**
 * Base classes and concrete classes share a parser AST node. We assert that
 * base classes and concrete classes are well formed in our checking phase.
 */
export interface ClassDeclaration extends NamedDeclaration {
  readonly type: DeclarationType.Class;
  readonly base: boolean;
  readonly unsealed: boolean;
  readonly typeParameters: ReadonlyArray<TypeParameter>;
  readonly parameters: ReadonlyArray<FunctionParameter>;
  readonly extends: ReferenceType | undefined;
  readonly implements: ReadonlyArray<ReferenceType>;
  readonly body: ReadonlyArray<Member>;
}

/**
 * Concrete methods, base methods, and interface methods all use the `Member`
 * interface. We assert that members are well formed in our checking phase.
 */
export type Member = MethodMember;

export interface NamedMember {
  readonly access: Access;
  readonly name: Name;
}

export interface MethodMember extends NamedMember {
  readonly base: boolean;
  readonly typeParameters: ReadonlyArray<TypeParameter>;
  readonly parameters: ReadonlyArray<FunctionParameter>;
  readonly return: Type | undefined;
  readonly body: Expression | undefined;
}

export interface InterfaceDeclaration extends NamedDeclaration {
  readonly type: DeclarationType.Interface;
  readonly typeParameters: ReadonlyArray<TypeParameter>;
  readonly extends: ReadonlyArray<ReferenceType>;
  readonly body: ReadonlyArray<Member>;
}

export type Type =
  | ReferenceType
  | UnitType
  | TupleType
  | RecordType
  | FunctionType
  | MemberType
  | GenericType
  | QuantifiedType
  | WrappedType;

export const enum TypeType {
  Reference,
  Unit,
  Tuple,
  Record,
  Function,
  Member,
  Generic,
  Quantified,
  Wrapped,
}

export interface ReferenceType extends Node {
  readonly type: TypeType.Reference;
  readonly identifier: Identifier;
}

export interface UnitType extends Node {
  readonly type: TypeType.Unit;
}

export interface TupleType extends Node {
  readonly type: TypeType.Tuple;
  readonly types: List2<Type>;
}

export interface RecordType extends Node {
  readonly type: TypeType.Record;
  readonly properties: ReadonlyArray<RecordTypeProperty>;
}

export type RecordTypeProperty = {
  readonly key: Name;
  readonly value: Type;
  readonly optional: boolean;
};

export interface FunctionType extends Node {
  readonly type: TypeType.Function;
  readonly parameters: ReadonlyArray<Type>;
  readonly body: Type;
}

export interface MemberType extends Node {
  readonly type: TypeType.Member;
  readonly namespace: Type;
  readonly member: Name;
}

export interface GenericType extends Node {
  readonly type: TypeType.Generic;
  readonly callee: Type;
  readonly typeArguments: ReadonlyArray<Type>;
}

export interface QuantifiedType extends Node {
  readonly type: TypeType.Quantified;
  readonly typeParameters: ReadonlyArray<TypeParameter>;
  readonly body: Type;
}

export interface WrappedType extends Node {
  readonly type: TypeType.Wrapped;
  readonly wrapped: Type;
}

export type TypeParameter = {
  readonly name: Name;
  readonly bounds: ReadonlyArray<Type>;
};

export type Statement =
  | ExpressionStatement
  | BindingStatement
  | WhileLoopStatement
  | ForLoopStatement;

export const enum StatementType {
  Expression,
  Binding,
  WhileLoop,
  ForLoop,
}

export interface ExpressionStatement extends Node {
  readonly type: StatementType.Expression;
  readonly expression: Expression;
}

export interface BindingStatement extends Node {
  readonly type: StatementType.Binding;
  readonly binding: Pattern;
  readonly annotation: Type | undefined;
  readonly value: Expression;
}

export interface WhileLoopStatement extends Node {
  readonly type: StatementType.WhileLoop;
  readonly test: Expression;
  readonly body: Expression;
}

export interface ForLoopStatement extends Node {
  readonly type: StatementType.ForLoop;
  readonly binding: Pattern;
  readonly annotation: Type | undefined;
  readonly iterable: Expression;
  readonly body: Expression;
}

export type Expression =
  | ReferenceExpression
  | UnitExpression
  | TupleExpression
  | RecordExpression
  | ListExpression
  | MemberExpression
  | CallExpression
  | FunctionExpression
  | ConditionalExpression
  | ReturnExpression
  | BreakExpression
  | ContinueExpression
  | LoopExpression
  | LogicalExpression
  | BinaryExpression
  | UnaryExpression
  | BlockExpression
  | WrappedExpression;

// TODO: Match expressions
export const enum ExpressionType {
  Reference,
  Unit,
  Tuple,
  Record,
  List,
  Member,
  Call,
  Function,
  Conditional,
  Return,
  Break,
  Continue,
  Loop,
  Logical,
  Binary,
  Unary,
  Block,
  Wrapped,
}

export interface ReferenceExpression extends Node {
  readonly type: ExpressionType.Reference;
  readonly identifier: Identifier;
}

export interface UnitExpression extends Node {
  readonly type: ExpressionType.Unit;
}

export interface TupleExpression extends Node {
  readonly type: ExpressionType.Tuple;
  readonly expressions: List2<Expression>;
}

export interface RecordExpression extends Node {
  readonly type: ExpressionType.Record;
  readonly extension: Expression | undefined;
  readonly properties: ReadonlyArray<RecordExpressionProperty>;
}

export type RecordExpressionProperty = {
  readonly key: Name;
  readonly annotation: Type | undefined;
  readonly value: Expression;
};

export interface ListExpression extends Node {
  readonly type: ExpressionType.List;
  readonly items: ReadonlyArray<Expression>;
}

export interface MemberExpression extends Node {
  readonly type: ExpressionType.Member;
  readonly namespace: Expression;
  readonly member: Identifier;
}

export interface CallExpression extends Node {
  readonly type: ExpressionType.Call;
  readonly callee: Expression;
  readonly arguments: ReadonlyArray<Expression>;
}

export interface FunctionExpression extends Node {
  readonly type: ExpressionType.Function;
  readonly parameters: ReadonlyArray<FunctionParameter>;
  readonly body: Expression;
}

export interface ConditionalExpression extends Node {
  readonly type: ExpressionType.Conditional;
  readonly test: Expression;
  readonly consequent: Expression;
  readonly alternate: Expression;
}

export interface ReturnExpression extends Node {
  readonly type: ExpressionType.Return;
  readonly argument: Expression | undefined;
}

export interface BreakExpression extends Node {
  readonly type: ExpressionType.Break;
  readonly argument: Expression | undefined;
}

export interface ContinueExpression extends Node {
  readonly type: ExpressionType.Continue;
}

export interface LoopExpression extends Node {
  readonly type: ExpressionType.Loop;
  readonly body: Expression;
}

/**
 * `LogicalExpression` is separate from `BinaryExpression` because the `right`
 * expression in `LogicalExpression` is evaluated conditionally and so deserves
 * special treatment.
 */
export interface LogicalExpression extends Node {
  readonly type: ExpressionType.Logical;
  readonly operator: LogicalExpressionOperator;
  readonly left: Expression;
  readonly right: Expression;
}

export const enum LogicalExpressionOperator {
  And = '&&',
  Or = '||',
}

export interface BinaryExpression {
  readonly type: ExpressionType.Binary;
  readonly operator: BinaryExpressionOperator;
  readonly left: Expression;
  readonly right: Expression;
}

export const enum BinaryExpressionOperator {
  Equal = '==',
  EqualNot = '!=',
  LessThan = '<',
  LessThanOrEqual = '<=',
  GreaterThan = '>',
  GreaterThanOrEqual = '>=',
  Add = '+',
  Subtract = '-',
  Multiply = '*',
  Divide = '/',
  Remainder = '%',
}

export interface UnaryExpression {
  readonly type: ExpressionType.Unary;
  readonly operator: UnaryExpressionOperator;
  readonly argument: Expression;
}

export const enum UnaryExpressionOperator {
  Negative = '-',
  Not = '!',
}

export interface BlockExpression {
  readonly type: ExpressionType.Block;
  readonly statements: ReadonlyArray<Statement>;
}

export interface WrappedExpression {
  readonly type: ExpressionType.Wrapped;
  readonly expression: Expression;
  readonly annotation: Type | undefined;
}
