# Function Declaration

FunctionDeclaration : Identifier Function

Function : GenericParameters? FunctionParameters FunctionReturnType? `->` FunctionBody

FunctionWithoutBody : GenericParameters? FunctionParameters FunctionReturnType

FunctionReturnType : TypeAnnotation

FunctionParameters[Constructor] : `(` FunctionParameterList[?Constructor]? `)`

FunctionParameterList[Constructor] :
  - FunctionParameter[?Constructor] `,`?
  - FunctionParameter[?Constructor] `,` FunctionParameterList[?Constructor]

FunctionParameter[Constructor] : Pattern[WithAnnotation, ?Constructor]

FunctionBody : Expression

Defines some reusable behavior on program data.

Functions are named with an {Identifier} and may be referenced before they are declared. This means function declarations are freely recursive.

The optional {GenericParameters} specify some parameters at the type level. These generic types and their bounds can specialize the function so the programmer can build behaviors which abstract over a category of types instead of a single type. The types from {GenericParameters} are in scope of the rest of the function declaration.

The required {FunctionParameters} specifies zero or more parameter values the function accepts. Function calls must provide an argument for all of these parameters at the callsite. Unlike in a functional programming language where parameters are “curried” so parameter lists are syntax sugar for functions returning functions.

Named parameters and optional parameters may be represented with records.

```ite example
myAdditionFunction(x: Int, y: Int) -> x + y

myAdditionFunctionWithNamedParameters({ x: Int, y: Int }) -> x + y
```

The optional {FunctionReturnType} allows the programmer to annotate the type returned by their function body {Expression}.

The behavior of the function is defined by its {FunctionBody}. All the parameters declared in {FunctionParameters} are in scope of the function body.

Note: {FunctionWithoutBody} is a convenience grammar not used in {FunctionDeclaration}. It is used in {ClassDeclaration} and {InterfaceDeclaration} to specify an unimplemented function.
