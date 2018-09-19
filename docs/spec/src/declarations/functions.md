# Function Declaration

FunctionDeclaration : Identifier Function

Function : GenericParameters? FunctionParameters FunctionReturnType? `->` FunctionBody

FunctionWithoutBody : GenericParameters? FunctionParameters FunctionReturnType

FunctionReturnType : `:` Type

FunctionParameters : `(` FunctionParameterList? `)`

FunctionParameterList :
  - FunctionExpressionParameter `,`?
  - FunctionExpressionParameter `,` FunctionParameterList

FunctionBody : Expression

Note: {FunctionWithoutBody} is a convenience grammar not used in {FunctionDeclaration}. It is used in {ClassDeclaration} and {InterfaceDeclaration} to specify an unimplemented function.

TODO: Add throw statement to {FunctionBody}.
