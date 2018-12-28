type 'a t = ('a * 'a list)

val create: 'a -> 'a list -> 'a t
val append: 'a t -> 'a t -> 'a t
val map: ('a -> 'b) -> 'a t -> 'b t
val fold_left: ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
val fold_right: ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
val to_list: 'a t -> 'a list
val from_list: 'a list -> 'a t
