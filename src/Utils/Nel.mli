type 'a t

val create: 'a -> 'a list -> 'a t
val map: ('a -> 'b) -> 'a t -> 'b t
val fold_left: ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
val to_list: 'a t -> 'a list
val from_list: 'a list -> 'a t
