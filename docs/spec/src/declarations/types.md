# Type Declaration

TypeDeclaration : `type` Identifier GenericParameters? `=` Type

Defines a named alias for {Type}. This named alias may be used anywhere in place of {Type}. The alias may optionally have some generic type parameters.

```ite example
type Point = {
  x: Int,
  y: Int,
}
```
