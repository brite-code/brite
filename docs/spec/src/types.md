# Types

Type :
  - ReferenceType
  - UnitType
  - TupleType
  - RecordType
  - FunctionType
  - WrappedType

WrappedType: `(` Type `)`

TypeAnnotation: `:` Type

Note: {TypeAnnotation} is a convenience grammar rule for type annotations which can be fairly common.

## Reference Type

ReferenceType : Identifier GenericArguments?

Note: Unlike {ReferenceExpression} we include arguments with {ReferenceType} since only named types may have generic parameters. Also generic type instantiation is pure unlike function calls.

## Unit Type

UnitType : `(` `)`

## Tuple Type

TupleType : `(` TupleTypeElementList `)`

TupleTypeElementList :
  - Type `,` Type `,`?
  - Type `,` TupleTypeElementList

## Record Type

RecordType : `{` RecordTypePropertyList? `}`

RecordTypePropertyList :
  - RecordTypeProperty `,`?
  - RecordTypeProperty `,` RecordTypePropertyList

RecordTypeProperty : Identifier `:` Type

## Function Type

FunctionType : GenericParameters? `(` FunctionTypeParameterList? `)` `->` Type

FunctionTypeParameterList :
  - Type `,`?
  - Type `,` FunctionTypeParameterList
