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
  readonly kind: unknown;
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

export const enum DeclarationKind {
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
  readonly kind: DeclarationKind.Type;
  readonly typeParameters: ReadonlyArray<TypeParameter>;
  readonly value: Type;
}

export interface FunctionDeclaration extends NamedDeclaration {
  readonly kind: DeclarationKind.Function;
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
  readonly kind: DeclarationKind.Class;
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
  readonly kind: DeclarationKind.Interface;
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

export const enum TypeKind {
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
  readonly kind: TypeKind.Reference;
  readonly identifier: Identifier;
}

export function ReferenceType(loc: Loc, identifier: Identifier): ReferenceType {
  return {kind: TypeKind.Reference, loc, identifier};
}

export interface UnitType extends Node {
  readonly kind: TypeKind.Unit;
}

export function UnitType(loc: Loc): UnitType {
  return {kind: TypeKind.Unit, loc};
}

export interface TupleType extends Node {
  readonly kind: TypeKind.Tuple;
  readonly elements: ReadonlyArray2<Type>;
}

export function TupleType(loc: Loc, elements: ReadonlyArray2<Type>): TupleType {
  return {kind: TypeKind.Tuple, loc, elements};
}

export interface RecordType extends Node {
  readonly kind: TypeKind.Record;
  readonly properties: ReadonlyArray<RecordTypeProperty>;
}

export function RecordType(
  loc: Loc,
  properties: ReadonlyArray<RecordTypeProperty>
): RecordType {
  return {kind: TypeKind.Record, loc, properties};
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
  readonly kind: TypeKind.Function;
  readonly parameters: ReadonlyArray<Type>;
  readonly body: Type;
}

export function FunctionType(
  loc: Loc,
  parameters: ReadonlyArray<Type>,
  body: Type
): FunctionType {
  return {kind: TypeKind.Function, loc, parameters, body};
}

export interface MemberType extends Node {
  readonly kind: TypeKind.Member;
  readonly namespace: Type;
  readonly member: Name;
}

export function MemberType(
  loc: Loc,
  namespace: Type,
  member: Name
): MemberType {
  return {kind: TypeKind.Member, loc, namespace, member};
}

export interface GenericType extends Node {
  readonly kind: TypeKind.Generic;
  readonly callee: Type;
  readonly typeArguments: ReadonlyArray<Type>;
}

export function GenericType(
  loc: Loc,
  callee: Type,
  typeArguments: ReadonlyArray<Type>
): GenericType {
  return {kind: TypeKind.Generic, loc, callee, typeArguments};
}

export interface QuantifiedType extends Node {
  readonly kind: TypeKind.Quantified;
  readonly typeParameters: ReadonlyArray<TypeParameter>;
  readonly body: Type;
}

export function QuantifiedType(
  loc: Loc,
  typeParameters: ReadonlyArray<TypeParameter>,
  body: Type
): QuantifiedType {
  return {kind: TypeKind.Quantified, loc, typeParameters, body};
}

export interface WrappedType extends Node {
  readonly kind: TypeKind.Wrapped;
  readonly type: Type;
}

export function WrappedType(loc: Loc, type: Type): WrappedType {
  return {kind: TypeKind.Wrapped, loc, type};
}

export interface ErrorType extends Node {
  readonly kind: TypeKind.Error;
  readonly error: ParserError;
}

export function ErrorType(loc: Loc, error: ParserError): ErrorType {
  return {kind: TypeKind.Error, loc, error};
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
  | WhileLoopStatement
  | ForLoopStatement;

export const enum StatementKind {
  Expression = 'Expression',
  Binding = 'Binding',
  BindingProperty = 'BindingProperty',
  Assignment = 'Assignment',
  WhileLoop = 'WhileLoop',
  ForLoop = 'ForLoop',
}

export interface ExpressionStatement {
  readonly kind: StatementKind.Expression;
  readonly expression: Expression;
}

export interface BindingStatement {
  readonly kind: StatementKind.Binding;
  readonly binding: Pattern;
  readonly type: Type | undefined;
  readonly value: Expression;
}

export interface BindingPropertyStatement {
  readonly kind: StatementKind.BindingProperty;
  readonly property: ReadonlyArray2<Name>;
  readonly value: Expression;
}

export interface WhileLoopStatement {
  readonly kind: StatementKind.WhileLoop;
  readonly test: Expression;
  readonly body: Expression;
}

export interface ForLoopStatement {
  readonly kind: StatementKind.ForLoop;
  readonly binding: Pattern;
  readonly type: Type | undefined;
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
  | PatternExpression
  | ReturnExpression
  | BreakExpression
  | ContinueExpression
  | LoopExpression
  | LogicalExpression
  | BinaryExpression
  | UnaryExpression
  | BlockExpression
  | WrappedExpression;

export const enum ExpressionKind {
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
  Pattern = 'Pattern',
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
  readonly kind: ExpressionKind.Reference;
  readonly identifier: Identifier;
}

export interface HoleExpression extends Node {
  readonly kind: ExpressionKind.Hole;
}

export interface UnitExpression extends Node {
  readonly kind: ExpressionKind.Unit;
}

export interface TupleExpression extends Node {
  readonly kind: ExpressionKind.Tuple;
  readonly expressions: ReadonlyArray2<Expression>;
}

export interface RecordExpression extends Node {
  readonly kind: ExpressionKind.Record;
  readonly extension: Expression | undefined;
  readonly properties: ReadonlyArray<RecordExpressionProperty>;
}

export type RecordExpressionProperty = {
  readonly key: Name;
  readonly value: Expression;
  readonly type: Type | undefined;
};

export interface ListExpression extends Node {
  readonly kind: ExpressionKind.List;
  readonly items: ReadonlyArray<Expression>;
}

export interface MemberExpression extends Node {
  readonly kind: ExpressionKind.Member;
  readonly namespace: Expression;
  readonly member: Identifier;
}

export interface CallExpression extends Node {
  readonly kind: ExpressionKind.Call;
  readonly callee: Expression;
  readonly arguments: ReadonlyArray<Expression>;
}

export interface FunctionExpression extends Node {
  readonly kind: ExpressionKind.Function;
  readonly parameters: ReadonlyArray<FunctionParameter>;
  readonly body: Expression;
}

export interface ConditionalExpression extends Node {
  readonly kind: ExpressionKind.Conditional;
  readonly test: Expression;
  readonly consequent: Expression;
  readonly alternate: Expression;
}

export interface MatchExpression extends Node {
  readonly kind: ExpressionKind.Match;
  readonly test: Expression;
  readonly cases: ReadonlyArray<MatchCase>;
}

export type MatchCase = {
  readonly binding: ReadonlyArray1<Pattern>;
  readonly test: Expression | undefined;
  readonly body: Expression;
};

export interface PatternExpression extends Node {
  readonly kind: ExpressionKind.Pattern;
  readonly left: Expression;
  readonly right: Pattern;
}

export interface ReturnExpression extends Node {
  readonly kind: ExpressionKind.Return;
  readonly argument: Expression | undefined;
}

export interface BreakExpression extends Node {
  readonly kind: ExpressionKind.Break;
  readonly argument: Expression | undefined;
}

export interface ContinueExpression extends Node {
  readonly kind: ExpressionKind.Continue;
}

export interface LoopExpression extends Node {
  readonly kind: ExpressionKind.Loop;
  readonly body: Expression;
}

/**
 * `LogicalExpression` is separate from `BinaryExpression` because the `right`
 * expression in `LogicalExpression` is evaluated conditionally and so deserves
 * special treatment.
 */
export interface LogicalExpression extends Node {
  readonly kind: ExpressionKind.Logical;
  readonly operator: LogicalExpressionOperator;
  readonly left: Expression;
  readonly right: Expression;
}

export const enum LogicalExpressionOperator {
  And = '&&',
  Or = '||',
}

export interface BinaryExpression extends Node {
  readonly kind: ExpressionKind.Binary;
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

export interface UnaryExpression extends Node {
  readonly kind: ExpressionKind.Unary;
  readonly operator: UnaryExpressionOperator;
  readonly argument: Expression;
}

export const enum UnaryExpressionOperator {
  Negative = '-',
  Not = '!',
}

export interface BlockExpression extends Node {
  readonly kind: ExpressionKind.Block;
  readonly statements: ReadonlyArray<Statement>;
}

export interface WrappedExpression extends Node {
  readonly kind: ExpressionKind.Wrapped;
  readonly expression: Expression;
  readonly type: Type | undefined;
}

export type Pattern =
  | BindingPattern
  | UnitPattern
  | TuplePattern
  | RecordPattern
  | ListPattern
  | QualifiedPattern
  | DeconstructPattern
  | AliasPattern
  | WrappedPattern;

export const enum PatternKind {
  Binding = 'Binding',
  Unit = 'Unit',
  Tuple = 'Tuple',
  Record = 'Record',
  List = 'List',
  Qualified = 'Qualified',
  Deconstruct = 'Deconstruct',
  Alias = 'Alias',
  Wrapped = 'Wrapped',
}

export interface BindingPattern extends Node {
  readonly kind: PatternKind.Binding;
  readonly identifier: Identifier;
}

export function BindingPattern(
  loc: Loc,
  identifier: Identifier
): BindingPattern {
  return {kind: PatternKind.Binding, loc, identifier};
}

export interface UnitPattern extends Node {
  readonly kind: PatternKind.Unit;
}

export function UnitPattern(loc: Loc): UnitPattern {
  return {kind: PatternKind.Unit, loc};
}

export interface TuplePattern extends Node {
  readonly kind: PatternKind.Tuple;
  readonly elements: ReadonlyArray2<Pattern>;
}

export function TuplePattern(
  loc: Loc,
  elements: ReadonlyArray2<Pattern>
): TuplePattern {
  return {kind: PatternKind.Tuple, loc, elements};
}

export interface RecordPattern extends Node {
  readonly kind: PatternKind.Record;
  readonly properties: ReadonlyArray<RecordPatternProperty>;
}

export function RecordPattern(
  loc: Loc,
  properties: ReadonlyArray<RecordPatternProperty>
): RecordPattern {
  return {kind: PatternKind.Record, loc, properties};
}

export type RecordPatternProperty = {
  readonly key: Name;
  readonly value: Pattern;
  readonly type: Type | undefined;
  readonly optional: boolean;
};

export interface ListPattern extends Node {
  readonly kind: PatternKind.List;
  readonly elements: ReadonlyArray<Pattern>;
}

export function ListPattern(
  loc: Loc,
  elements: ReadonlyArray<Pattern>
): ListPattern {
  return {kind: PatternKind.List, loc, elements};
}

export interface QualifiedPattern extends Node {
  readonly kind: PatternKind.Qualified;
  readonly identifiers: ReadonlyArray2<Name>;
}

export function QualifiedPattern(
  loc: Loc,
  identifiers: ReadonlyArray2<Name>
): QualifiedPattern {
  return {kind: PatternKind.Qualified, loc, identifiers};
}

export interface DeconstructPattern extends Node {
  readonly kind: PatternKind.Deconstruct;
  readonly callee: ReadonlyArray1<Name>;
  readonly arguments: ReadonlyArray<Pattern>;
}

export function DeconstructPattern(
  loc: Loc,
  callee: ReadonlyArray1<Name>,
  args: ReadonlyArray<Pattern>
): DeconstructPattern {
  return {kind: PatternKind.Deconstruct, loc, callee, arguments: args};
}

export interface AliasPattern extends Node {
  readonly kind: PatternKind.Alias;
  readonly alias: Identifier;
  readonly pattern: Pattern;
}

export function AliasPattern(
  loc: Loc,
  alias: Identifier,
  pattern: Pattern
): AliasPattern {
  return {kind: PatternKind.Alias, loc, alias, pattern};
}

export interface WrappedPattern extends Node {
  readonly kind: PatternKind.Wrapped;
  readonly pattern: Pattern;
  readonly type: Type | undefined;
}

export function WrappedPattern(
  loc: Loc,
  pattern: Pattern,
  type: Type | undefined
): WrappedPattern {
  return {kind: PatternKind.Wrapped, loc, pattern, type};
}
