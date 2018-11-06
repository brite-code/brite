(* NOTE: This is a temporary hack parser for parsing language constructs based
 * on academic traditions. For example, functions are parsed with the lambda
 * character: `λx.x`. It does very little for the user experience. We will write
 * a more complete parser later. *)

type glyph =
  | Arrow
  | Bottom
  | Colon
  | Comma
  | Dot
  | EmptySet
  | Equals
  | ForAll
  | Lambda
  | LessThanOrEqual
  | ParenthesesLeft
  | ParenthesesRight

  (* Keywords *)
  | Let
  | In
  | True
  | False
  | Boolean
  | Number

type token =
  | Identifier of string
  | Number of float
  | Glyph of glyph

(* Converts a stream of characters into a stream of tokens. *)
let tokenize cs =
  Stream.from (fun _ -> (
    let rec loop () =
      match Stream.peek cs with
      | None -> None
      | Some c -> (
        Stream.junk cs;
        match c with
        | ':' -> Some (Glyph Colon)
        | ',' -> Some (Glyph Comma)
        | '.' -> Some (Glyph Dot)
        | '=' -> Some (Glyph Equals)
        | '(' -> Some (Glyph ParenthesesLeft)
        | ')' -> Some (Glyph ParenthesesRight)

        | '\226' -> (
          match Stream.next cs, Stream.next cs with
          | '\134', '\146' -> Some (Glyph Arrow)
          | '\136', '\128' -> Some (Glyph ForAll)
          | '\136', '\133' -> Some (Glyph EmptySet)
          | '\137', '\165' -> Some (Glyph LessThanOrEqual)
          | '\138', '\165' -> Some (Glyph Bottom)
          | _, _ -> raise (Stream.Error "Unexpected byte.")
        )

        | '\206' -> (
          match Stream.next cs with
          | '\187' -> Some (Glyph Lambda)
          | _ -> raise (Stream.Error "Unexpected byte.")
        )

        | ' ' | '\n' | '\t' | '\r' -> loop ()

        | 'a' .. 'z' | '_' -> (
          let rec loop n name =
            match Stream.peek cs with
            | Some ('a' .. 'z' as c)
            | Some ('A' .. 'Z' as c)
            | Some ('0' .. '9' as c)
            | Some ('_' as c)
              -> Stream.junk cs; loop (n + 1) (c :: name)

            | _ -> (n, name)
          in
          let (length, name) = loop 1 [c] in
          let name = ref (List.rev name) in
          let name = String.init length (fun _ -> let c = List.hd !name in name := List.tl !name; c) in
          match name with
          | "let" -> Some (Glyph Let)
          | "in" -> Some (Glyph In)
          | "true" -> Some (Glyph True)
          | "false" -> Some (Glyph False)
          | "boolean" -> Some (Glyph Boolean)
          | "number" -> Some (Glyph Number)
          | name -> Some (Identifier name)
        )

        | '0' .. '9' -> (
          let rec loop n number =
            match Stream.peek cs with
            | Some ('0' .. '9' as c) -> Stream.junk cs; loop (n + 1) (c :: number)
            | _ -> (n, number)
          in
          let (length, number) = loop 1 [c] in
          let (length, number) =
            if Stream.peek cs = Some '.' then (
              Stream.junk cs; loop (length + 1) ('.' :: number)
            ) else (
              (length, number)
            )
          in
          let number = ref (List.rev number) in
          let number = String.init length (fun _ -> let c = List.hd !number in number := List.tl !number; c) in
          let number = float_of_string number in
          Some (Number number)
        )

        | c -> raise (Stream.Error ("Unexpected char: '" ^ Char.escaped c ^ "'"))
      )
    in
    loop ()
  ))

let parse_glyph tokens glyph =
  if not (Stream.next tokens = Glyph glyph) then raise (Stream.Error "Unexpected token.")

let parse_identifier tokens =
  match Stream.next tokens with
  | Identifier name -> name
  | _ -> raise (Stream.Error "Unexpected token.")

let parse_non_empty_comma_list tokens parse_item =
  let rec loop () =
    let item = parse_item tokens in
    let list = match Stream.peek tokens with
    | Some (Glyph Comma) -> Stream.junk tokens; loop ()
    | _ -> []
    in
    item :: list
  in
  loop ()

let rec parse_monotype tokens =
  let type_ = parse_unwrapped_monotype tokens in
  match Stream.peek tokens with
  | Some (Glyph Arrow) ->
    Stream.junk tokens;
    let body = parse_monotype tokens in
    Type.function_ type_ body

  | _ -> type_

and parse_unwrapped_monotype tokens =
  match Stream.next tokens with
  | Glyph ParenthesesLeft ->
    let type_ = parse_monotype tokens in
    parse_glyph tokens ParenthesesRight;
    type_

  | Glyph Boolean -> Type.boolean
  | Glyph Number -> Type.number

  | Identifier name -> Type.variable name

  | _ -> raise (Stream.Error "Expected monotype.")

let rec parse_polytype tokens =
  match Stream.peek tokens with
  | Some (Glyph ForAll) -> (
    Stream.junk tokens;
    let rec loop () =
      let bounds = match Stream.next tokens with
      | Identifier name -> [(name, Type.unbounded)]
      | Glyph ParenthesesLeft -> (
        let bounds = parse_non_empty_comma_list tokens parse_bound in
        parse_glyph tokens ParenthesesRight;
        bounds
      )
      | _ -> raise (Stream.Error "Unexpected token.")
      in
      parse_glyph tokens Dot;
      match Stream.peek tokens with
      | Some (Glyph ForAll) -> Stream.junk tokens; bounds :: loop ()
      | _ -> [bounds]
    in
    let bounds = List.concat (loop ()) in
    let body = parse_monotype tokens in
    Type.quantify bounds body
  )

  | Some (Glyph Bottom) -> Stream.junk tokens; Type.bottom

  | _ -> Type.to_polytype (parse_monotype tokens)

and parse_bound tokens =
  let name = parse_identifier tokens in
  match Stream.peek tokens with
  | Some (Glyph Equals) ->
    Stream.junk tokens;
    let bound_type = parse_polytype tokens in
    (name, Type.bound Rigid bound_type)
  | Some (Glyph LessThanOrEqual) ->
    Stream.junk tokens;
    let bound_type = parse_polytype tokens in
    (name, Type.bound Flexible bound_type)
  | _ ->
    (name, Type.unbounded)

let parse_prefix tokens =
  parse_glyph tokens ParenthesesLeft;
  match Stream.peek tokens with
  | Some (Glyph EmptySet) ->
    Stream.junk tokens;
    parse_glyph tokens ParenthesesRight;
    []
  | _ ->
    let bounds = parse_non_empty_comma_list tokens parse_bound in
    parse_glyph tokens ParenthesesRight;
    bounds

let rec parse_expression tokens =
  match Stream.peek tokens with
  | Some (Glyph Lambda) ->
    Stream.junk tokens;
    let parameter = parse_identifier tokens in
    parse_glyph tokens Dot;
    let body = parse_expression tokens in
    Expression.function_ parameter body

  | Some (Glyph Let) ->
    Stream.junk tokens;
    let name = parse_identifier tokens in
    parse_glyph tokens Equals;
    let value = parse_expression tokens in
    parse_glyph tokens In;
    let body = parse_expression tokens in
    Expression.binding name value body

  | _ -> (
    let e = match try_parse_unwrapped_expression tokens with
    | Some e -> e
    | None -> raise (Stream.Error "Expected expression.")
    in
    let rec loop e1 =
      match try_parse_unwrapped_expression tokens with
      | Some e2 -> loop (Expression.call e1 e2)
      | None -> e1
    in
    loop e
  )

and try_parse_unwrapped_expression tokens =
  match Stream.peek tokens with
  | Some (Glyph ParenthesesLeft) ->
    Stream.junk tokens;
    let expression = parse_expression tokens in
    if Stream.peek tokens = Some (Glyph Colon) then (
      Stream.junk tokens;
      let type_ = parse_polytype tokens in
      parse_glyph tokens ParenthesesRight;
      Some (Expression.annotation expression type_)
    ) else (
      parse_glyph tokens ParenthesesRight;
      Some expression
    )

  | Some (Identifier name) ->
    Stream.junk tokens;
    Some (Expression.variable name)

  | Some (Number number) ->
    Stream.junk tokens;
    Some (Expression.number number)

  | Some (Glyph True) ->
    Stream.junk tokens;
    Some (Expression.boolean true)

  | Some (Glyph False) ->
    Stream.junk tokens;
    Some (Expression.boolean false)

  | _ -> None

let parse_context tokens =
  parse_glyph tokens ParenthesesLeft;
  match Stream.peek tokens with
  | Some (Glyph EmptySet) ->
    Stream.junk tokens;
    parse_glyph tokens ParenthesesRight;
    []
  | _ ->
    let entries = parse_non_empty_comma_list tokens (fun tokens -> (
      let name = parse_identifier tokens in
      parse_glyph tokens Colon;
      let type_ = parse_polytype tokens in
      (name, type_)
    )) in
    parse_glyph tokens ParenthesesRight;
    entries
