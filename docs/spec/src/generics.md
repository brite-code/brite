# Generics

## Generic Parameters

GenericParameters : `<` GenericParameterList `>`

GenericParameterList :
  - [empty]
  - GenericParameter `,`?
  - GenericParameter `,` GenericParameterList

GenericParameter :
  - Identifier
  - Identifier `:` GenericParameterBoundList

GenericParameterBoundList :
  - Type
  - Type `+` GenericParameterBoundList

## Generic Arguments

GenericArguments : `<` GenericArgumentList `>`

GenericArgumentList :
  - [empty]
  - Type `,`?
  - Type `,` GenericArgumentList
