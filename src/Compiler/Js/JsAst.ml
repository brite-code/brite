(* This module represents the JavaScript AST. It is based on the Babel AST
 * specified here:
 *
 * https://github.com/babel/babel/blob/master/packages/babel-parser/ast/spec.md *)

type identifier = string

type variable_declaration_kind =
  | Var
  | Let
  | Const

type statement =
  | ExpressionStatement of { expression: expression }
  | ReturnStatement of { argument: expression }
  | IfStatement of { test: expression; consequent: block_statement; alternate: block_statement }
  | VariableDeclaration of { kind: variable_declaration_kind; id: pattern; init: expression option }

and block_statement = { statements: statement list }

and expression =
  | IdentifierExpression of identifier
  | BooleanLiteral of { value: bool }
  | NumericLiteral of { value: float }
  | ArrowFunctionExpression of { params: pattern list; body: arrow_function_body }
  | AssignmentExpression of { left: pattern; right: expression }
  | ConditionalExpression of { test: expression; consequent: expression; alternate: expression }
  | CallExpression of { callee: expression; arguments: expression list }

and arrow_function_body =
  | ExpressionArrowFunctionBody of expression
  | BlockStatementArrowFunctionBody of block_statement

and pattern =
  | IdentifierPattern of identifier [@@unboxed]

let identifier value = value
let expression_statement expression = ExpressionStatement { expression }
let return_statement argument = ReturnStatement { argument }
let if_statement test consequent alternate =
  IfStatement { test; consequent = { statements = consequent }; alternate = { statements = alternate } }
let variable_declaration kind id init  = VariableDeclaration { kind; id; init }
let identifier_expression identifier = IdentifierExpression identifier
let boolean_literal value = BooleanLiteral { value }
let numeric_literal value = NumericLiteral { value }
let arrow_function_expression params body = ArrowFunctionExpression { params; body }
let assignment_expression left right = AssignmentExpression { left; right }
let conditional_expression test consequent alternate = ConditionalExpression { test; consequent; alternate }
let call_expression callee arguments = CallExpression { callee; arguments }
let identifier_pattern identifier = IdentifierPattern identifier

(* Converts a list of statements to an arrow function body. If there is only a
 * single return statement then that becomes an expression body. *)
let statements_to_arrow_function_body statements =
  match statements with
  | [ReturnStatement { argument }] -> ExpressionArrowFunctionBody argument
  | _ -> BlockStatementArrowFunctionBody { statements }

(* If the statement is an assignment expression statement assigning to the
 * provided name then we return the value being assigned. Otherwise we
 * return nothing. *)
let assignment_to statement name =
  match statement with
  | ExpressionStatement { expression = AssignmentExpression { left; right } }
      when left = IdentifierPattern name ->
    Some right
  | _ -> None
