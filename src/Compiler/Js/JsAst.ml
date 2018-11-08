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

type program = {
  program_statements: statement list;
}

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
let program statements = { program_statements = statements }

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

(* The expression precedence levels as defined by the ECMAScript specification’s
 * expression grammar. This helps us determine when we need to
 * wrap expressions. *)
type precedence =
  | Top
  | Assignment
  | Conditional
  | LogicalOr
  | LogicalAnd
  | BitwiseOr
  | BitwiseXor
  | BitwiseAnd
  | Equality
  | Relational
  | Shift
  | Additive
  | Multiplicative
  | Exponential
  | Unary
  | Update
  | Call
  | Member
  | Primary
  | Bottom

let int_of_precedence = function
  | Top -> 0
  | Assignment -> 1
  | Conditional -> 2
  | LogicalOr -> 3
  | LogicalAnd -> 4
  | BitwiseOr -> 5
  | BitwiseXor -> 6
  | BitwiseAnd -> 7
  | Equality -> 8
  | Relational -> 9
  | Shift -> 10
  | Additive -> 11
  | Multiplicative -> 12
  | Exponential -> 13
  | Unary -> 14
  | Update -> 15
  | Call -> 16
  | Member -> 17
  | Primary -> 18
  | Bottom -> 19

let compare_precedence a b = int_of_precedence a - int_of_precedence b

(* Prints the AST of a JavaScript statement to a string. *)
let rec print_statement indentation statement =
  match statement with
  | ExpressionStatement { expression } ->
    let expression = print_expression indentation Top expression in (* TODO: Objects must be wrapped *)
    expression ^ ";"

  | ReturnStatement { argument } ->
    let argument = print_expression indentation Top argument in
    "return " ^ argument ^ ";"

  | IfStatement { test; consequent; alternate } ->
    let test = print_expression indentation Top test in
    let consequent = print_block_statement indentation consequent in
    let alternate = print_block_statement indentation alternate in
    "if (" ^ test ^ ") " ^ consequent ^ " else " ^ alternate

  | VariableDeclaration { kind; id; init } -> (
    let kind = match kind with
    | Var -> "var"
    | Let -> "let"
    | Const -> "const"
    in
    let id = print_pattern id in
    match init with
    | None -> kind ^ " " ^ id ^ ";"
    | Some init ->
      let init = print_expression indentation Top init in
      kind ^ " " ^ id ^ " = " ^ init ^ ";"
  )

(* Prints the AST of a JavaScript block statement to a string. *)
and print_block_statement indentation { statements } =
  match statements with
  | [] -> "{}"
  | statements ->
    let indentation' = indentation ^ "  " in
    let statements = List.map (fun statement ->
      indentation' ^ print_statement indentation statement ^ "\n"
    ) statements in
    let statements = String.concat "" statements in
    "{\n" ^ statements ^ indentation ^ "}"

(* Prints the AST of a JavaScript expression to a string. *)
and print_expression indentation precedence expression =
  let (expression_precedence, expression) = match expression with
  | IdentifierExpression identifier -> (Primary, identifier)
  | BooleanLiteral { value } -> (Primary, if value then "true" else "false")
  | NumericLiteral { value } -> (Primary, string_of_float value)

  | ArrowFunctionExpression { params; body } ->
    let params = match params with
    | [IdentifierPattern name] -> name
    | _ ->
      let params = List.map print_pattern params in
      let params = String.concat ", " params in
      "(" ^ params ^ ")"
    in
    let body = match body with
    | ExpressionArrowFunctionBody expression -> print_expression indentation Assignment expression (* TODO: Objects must be wrapped *)
    | BlockStatementArrowFunctionBody block_statement -> print_block_statement indentation block_statement
    in
    (Assignment, params ^ " => " ^ body)

  | AssignmentExpression { left; right } ->
    let left = print_pattern left in
    let right = print_expression indentation Assignment right in
    (Assignment, left ^ " = " ^ right)

  | ConditionalExpression { test; consequent; alternate } ->
    let test = print_expression indentation LogicalOr test in
    let consequent = print_expression indentation Assignment consequent in
    let alternate = print_expression indentation Assignment alternate in
    (Conditional, test ^ " ? " ^ consequent ^ " : " ^ alternate)

  | CallExpression { callee; arguments } ->
    let callee = print_expression indentation Call callee in
    let arguments = List.map (print_expression indentation Assignment) arguments in
    let arguments = String.concat ", " arguments in
    (Call, callee ^ "(" ^ arguments ^ ")")
  in
  if compare_precedence expression_precedence precedence < 0 then "(" ^ expression ^ ")" else expression

(* Prints the AST of a JavaScript pattern to a string. *)
and print_pattern pattern =
  match pattern with
  | IdentifierPattern identifier -> identifier

(* Prints the AST of a JavaScript program to a string. *)
let print_program { program_statements } =
  let statements = List.map (fun statement -> print_statement "" statement ^ "\n") program_statements in
  String.concat "" statements
