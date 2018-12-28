type 'a t =
  | Empty
  | Cons of 'a * 'a t
  | ConsEnd of 'a t * 'a

val concat: 'a t -> 'a t -> 'a t
val to_list: 'a t -> 'a list
