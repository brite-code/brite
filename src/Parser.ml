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
  | String

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
          | "string" -> Some (Glyph String)
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
  | Glyph String -> Type.string

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
        let bounds = parse_non_empty_comma_list tokens (fun tokens ->
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
        ) in
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
