# Generics

## Generic Parameters

GenericParameters : `<` GenericParameterList `>`

GenericParameterList :
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
  - GenericArgument `,`?
  - GenericArgument `,` GenericArgumentList

GenericArgument : Type
