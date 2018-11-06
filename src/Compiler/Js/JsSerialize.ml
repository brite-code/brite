type eager_or_lazy =
  | Eager
  | Lazy

type statements =
  | Empty
  | Only of JsAst.statement
  | Concat of statements * statements

let concat_statements a b =
  match a, b with
  | Empty, _ -> b
  | _, Empty -> a
  | _, _ -> Concat (a, b)

let serialize scope expression =
  match expression.Expression.description with
  | Variable { name } -> StringMap.find name scope
