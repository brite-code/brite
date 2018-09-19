# Declarations

Declaration : DeclarationAccess? DeclarationCommon

DeclarationCommon :
  - TypeDeclaration
  - FunctionDeclaration
  - ClassDeclaration
  - BaseClassDeclaration
  - InterfaceDeclaration

DeclarationAccess :
  - `public`
  - `protected`
  - `private`

Declarations are the main organizational tool for Brite programs. A {TypeDeclaration} allows the programmer to describe the shape of some data in their program. A {FunctionDeclaration} allows the programmer to describe some behavior on their program data. A {ClassDeclaration} allows the programmer to create a named data structure with some associated behaviors.

There are more declarations but conceptually {TypeDeclaration}, {FunctionDeclaration}, and {ClassDeclaration} form the core of the language. Declarations like {BaseClassDeclaration} and {InterfaceDeclaration} introduce polymorphism into the language.

By default, all declarations are private to the file which defined them. By adding `public` to a declaration the programmer makes it accessible to all Brite code. By adding `protected` to a declaration the programmer makes it accessible only to code in the same folder. Including files in nested folders.

Note: The behavior for `protected` in Brite is different then in most object oriented languages! In Brite `protected` applies to the accessibility in a file tree, *not* in an inheritance hierarchy.

Note: To make the default access level explicit one may use the `private` {DeclarationAccess} modifier.

Declarations are separated by {LineSeparator}.

```ite example
type Foo = Int

class Bar {}
```

[Types](types.md)

[Functions](functions.md)

[Classes](classes.md)

[Interfaces](interfaces.md)
