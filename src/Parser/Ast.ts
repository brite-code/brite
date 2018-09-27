import {ReadonlyArray1, ReadonlyArray2} from '../Utils/ArrayN';

import {ParserError} from './Error';
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

export function Name(loc: Loc, identifier: Identifier): Name {
  return {loc, identifier};
}

export type Declaration =
  | TypeDeclaration
  | FunctionDeclaration
  | ClassDeclaration
  | InterfaceDeclaration;

export const enum DeclarationType {
  Type = 'Type',
  Function = 'Function',
  Class = 'Class',
  Interface = 'Interface',
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
  | WrappedType
  | ErrorType;

export const enum TypeType {
  Reference = 'Reference',
  Unit = 'Unit',
  Tuple = 'Tuple',
  Record = 'Record',
  Function = 'Function',
  Member = 'Member',
  Generic = 'Generic',
  Quantified = 'Quantified',
  Wrapped = 'Wrapped',
  Error = 'Error',
}

export interface ReferenceType extends Node {
  readonly type: TypeType.Reference;
  readonly identifier: Identifier;
}

export function ReferenceType(loc: Loc, identifier: Identifier): ReferenceType {
  return {type: TypeType.Reference, loc, identifier};
}

export interface UnitType extends Node {
  readonly type: TypeType.Unit;
}

export function UnitType(loc: Loc): UnitType {
  return {type: TypeType.Unit, loc};
}

export interface TupleType extends Node {
  readonly type: TypeType.Tuple;
  readonly elements: ReadonlyArray2<Type>;
}

export function TupleType(loc: Loc, elements: ReadonlyArray2<Type>): TupleType {
  return {type: TypeType.Tuple, loc, elements};
}

export interface RecordType extends Node {
  readonly type: TypeType.Record;
  readonly properties: ReadonlyArray<RecordTypeProperty>;
}

export function RecordType(
  loc: Loc,
  properties: ReadonlyArray<RecordTypeProperty>
): RecordType {
  return {type: TypeType.Record, loc, properties};
}

export type RecordTypeProperty = {
  readonly key: Name;
  readonly value: Type;
  readonly optional: boolean;
};

export function RecordTypeProperty(key: Name, value: Type): RecordTypeProperty {
  return {key, value, optional: false};
}

export namespace RecordTypeProperty {
  export function optional(key: Name, value: Type): RecordTypeProperty {
    return {key, value, optional: true};
  }
}

export interface FunctionType extends Node {
  readonly type: TypeType.Function;
  readonly parameters: ReadonlyArray<Type>;
  readonly body: Type;
}

export function FunctionType(
  loc: Loc,
  parameters: ReadonlyArray<Type>,
  body: Type
): FunctionType {
  return {type: TypeType.Function, loc, parameters, body};
}

export interface MemberType extends Node {
  readonly type: TypeType.Member;
  readonly namespace: Type;
  readonly member: Name;
}

export function MemberType(
  loc: Loc,
  namespace: Type,
  member: Name
): MemberType {
  return {type: TypeType.Member, loc, namespace, member};
}

export interface GenericType extends Node {
  readonly type: TypeType.Generic;
  readonly callee: Type;
  readonly typeArguments: ReadonlyArray<Type>;
}

export function GenericType(
  loc: Loc,
  callee: Type,
  typeArguments: ReadonlyArray<Type>
): GenericType {
  return {type: TypeType.Generic, loc, callee, typeArguments};
}

export interface QuantifiedType extends Node {
  readonly type: TypeType.Quantified;
  readonly typeParameters: ReadonlyArray<TypeParameter>;
  readonly body: Type;
}

export function QuantifiedType(
  loc: Loc,
  typeParameters: ReadonlyArray<TypeParameter>,
  body: Type
): QuantifiedType {
  return {type: TypeType.Quantified, loc, typeParameters, body};
}

export interface WrappedType extends Node {
  readonly type: TypeType.Wrapped;
  readonly wrapped: Type;
}

export function WrappedType(loc: Loc, wrapped: Type): WrappedType {
  return {type: TypeType.Wrapped, loc, wrapped};
}

export interface ErrorType extends Node {
  readonly type: TypeType.Error;
  readonly error: ParserError;
}

export function ErrorType(loc: Loc, error: ParserError): ErrorType {
  return {type: TypeType.Error, loc, error};
}

export type TypeParameter = {
  readonly name: Name;
  readonly bounds: ReadonlyArray<Type>;
};

export function TypeParameter(
  name: Name,
  bounds: ReadonlyArray<Type>
): TypeParameter {
  return {name, bounds};
}

export type Statement =
  | ExpressionStatement
  | BindingStatement
  | BindingPropertyStatement
  | AssignmentStatement
  | WhileLoopStatement
  | ForLoopStatement;

export const enum StatementType {
  Expression = 'Expression',
  Binding = 'Binding',
  BindingProperty = 'BindingProperty',
  Assignment = 'Assignment',
  WhileLoop = 'WhileLoop',
  ForLoop = 'ForLoop',
}

export interface ExpressionStatement {
  readonly type: StatementType.Expression;
  readonly expression: Expression;
}

export interface BindingStatement {
  readonly type: StatementType.Binding;
  readonly binding: Pattern;
  readonly annotation: Type | undefined;
  readonly value: Expression;
}

export interface BindingPropertyStatement {
  readonly type: StatementType.BindingProperty;
  readonly property: ReadonlyArray2<Name>;
  readonly value: Expression;
}

export interface AssignmentStatement {
  readonly type: StatementType.Assignment;
  readonly reference: ReadonlyArray1<Name>;
  readonly value: Expression;
}

export interface WhileLoopStatement {
  readonly type: StatementType.WhileLoop;
  readonly test: Expression;
  readonly body: Expression;
}

export interface ForLoopStatement {
  readonly type: StatementType.ForLoop;
  readonly binding: Pattern;
  readonly annotation: Type | undefined;
  readonly iterable: Expression;
  readonly body: Expression;
}

export type Expression =
  | ReferenceExpression
  | HoleExpression
  | UnitExpression
  | TupleExpression
  | RecordExpression
  | ListExpression
  | MemberExpression
  | CallExpression
  | FunctionExpression
  | ConditionalExpression
  | MatchExpression
  | ReturnExpression
  | BreakExpression
  | ContinueExpression
  | LoopExpression
  | LogicalExpression
  | BinaryExpression
  | UnaryExpression
  | BlockExpression
  | WrappedExpression;

export const enum ExpressionType {
  Reference = 'Reference',
  Hole = 'Hole',
  Unit = 'Unit',
  Tuple = 'Tuple',
  Record = 'Record',
  List = 'List',
  Member = 'Member',
  Call = 'Call',
  Function = 'Function',
  Conditional = 'Conditional',
  Match = 'Match',
  Return = 'Return',
  Break = 'Break',
  Continue = 'Continue',
  Loop = 'Loop',
  Logical = 'Logical',
  Binary = 'Binary',
  Unary = 'Unary',
  Block = 'Block',
  Wrapped = 'Wrapped',
}

export interface ReferenceExpression extends Node {
  readonly type: ExpressionType.Reference;
  readonly identifier: Identifier;
}

export interface HoleExpression extends Node {
  readonly type: ExpressionType.Hole;
}

export interface UnitExpression extends Node {
  readonly type: ExpressionType.Unit;
}

export interface TupleExpression extends Node {
  readonly type: ExpressionType.Tuple;
  readonly expressions: ReadonlyArray2<Expression>;
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

export interface MatchExpression extends Node {
  readonly type: ExpressionType.Match;
  readonly test: Expression;
  readonly cases: ReadonlyArray<MatchCase>;
}

export type MatchCase = {
  readonly binding: ReadonlyArray1<Pattern>;
  readonly test: Expression | undefined;
  readonly body: Expression;
};

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
