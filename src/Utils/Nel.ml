(* Non-empty list *)

type 'a t = ('a * 'a list)

let create x xs = (x, xs)

let map f (x, xs) = (f x, List.map f xs)

let fold_left f acc (x, xs) = List.fold_left f (f acc x) xs

let to_list (x, xs) = x :: xs

let from_list xs =
  match xs with
  | [] -> assert false
  | x :: xs -> (x, xs)
