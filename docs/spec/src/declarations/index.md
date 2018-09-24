# Declarations

Declaration : Access? NamedDeclaration

NamedDeclaration :
  - TypeDeclaration
  - FunctionDeclaration
  - ClassDeclaration
  - BaseClassDeclaration
  - InterfaceDeclaration

Declarations are the main organizational tool for Brite programs. A {TypeDeclaration} allows the programmer to describe the shape of some data in their program. A {FunctionDeclaration} allows the programmer to describe some behavior on their program data. A {ClassDeclaration} allows the programmer to create a named data structure with some associated behaviors.

There are more declarations but conceptually {TypeDeclaration}, {FunctionDeclaration}, and {ClassDeclaration} form the core of the language. Declarations like {BaseClassDeclaration} and {InterfaceDeclaration} introduce polymorphism into the language.

Declaration accessibility in a Brite program is specified by its {Access} modifier.

[Types](types.md)

[Functions](functions.md)

[Classes](classes.md)

[Interfaces](interfaces.md)
