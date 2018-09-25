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
  readonly type: Type | undefined;
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
  readonly type1: Type;
  readonly type2: Type;
  readonly types: ReadonlyArray<Type>;
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
