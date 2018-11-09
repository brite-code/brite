(* Double-ended list data structure. *)

type 'a t =
  | Empty
  | Cons of 'a * 'a t
  | ConsEnd of 'a t * 'a

(* Concatenates two double-ended lists together. Not tail-recursive for the
 * number of items in `a`. *)
let concat a b =
  match a, b with
  | Empty, _ -> b
  | _, Empty -> a
  | _, _ ->
    let rec loop a b =
      match a with
      | Empty -> b
      | Cons (x, xs) -> Cons (x, loop xs b)
      | ConsEnd (xs, x) -> loop xs (Cons (x, b))
    in
    loop a b

(* Converts our double-ended list into a proper list. Not tail recursive. *)
let to_list list =
  let rec loop acc list =
    match list with
    | Empty -> acc
    | Cons (x, xs) -> x :: loop acc xs
    | ConsEnd (xs, x) -> loop (x :: acc) xs
  in
  loop [] list
