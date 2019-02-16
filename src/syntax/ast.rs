struct Identifier(String);

struct Module {
    declarations: Vec<Declaration>,
}

enum Declaration {
    Class(ClassDeclaration),
}

struct ClassDeclaration {
    name: Identifier,
    members: Vec<ClassMember>,
}

enum ClassMember {
    Field(ClassFieldMember),
    Method(ClassMethodMember),
}

struct ClassFieldMember {
    name: Identifier,
    type_: Type,
}

struct ClassMethodMember {
    name: Identifier,
    parameters: Vec<FunctionParameter>,
    body: Block,
}

struct FunctionParameter {
    pattern: Pattern,
    type_: Type,
}

struct Block {
    statements: Vec<Statement>,
}

enum Statement {
    Expression(Expression),
    Binding(BindingStatement),
}

struct BindingStatement {}

enum Expression {
    Reference(Identifier),
}

enum Pattern {
    Binding(Identifier),
    This,
}

enum Type {
    Reference(Identifier),
}
