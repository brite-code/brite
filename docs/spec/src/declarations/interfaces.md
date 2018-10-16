# Interface Declaration

InterfaceDeclaration : `interface` Identifier GenericParameters? InterfaceExtendsList InterfaceBody?

InterfaceBody : `{` InterfaceMemberList `}`

InterfaceExtendsList :
  - [empty]
  - InterfaceExtends
  - InterfaceExtendsList InterfaceExtends

InterfaceExtends : `extends` GenericParameters? Type

InterfaceMemberList :
  - [empty]
  - InterfaceMember LineSeparator?
  - InterfaceMember LineSeparator InterfaceMember

InterfaceMember :
  - InterfaceMethod

InterfaceMethod :
  - Access? Identifier Function
  - Access? Identifier FunctionWithoutBody

If {InterfaceMethod} does not have an {Access} modifier then it defaults to protected. An {InterfaceMethod} may have a private {Access} modifier, but the method may only be overwritten in a scope with access to the method.

Note: A Brite interface isn’t your typical Object-Oriented Programming interface that you would find in Java or C#. Instead Brite interfaces are statically resolved by the compiler instead of dynamically resolved at runtime  like Rust traits or Haskell type-classes. The programmer uses base classes for dynamic function dispatch.

Note: We call this “interface” instead of “trait” since “trait” implies the construct usually brings in some significant behaviors to a class. The most common interfaces, in fact, provide no extra behaviors. Like `Equality`.
