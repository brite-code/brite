type t

val collect: (unit -> 'a) -> ('a * t list)
val report_error: Error.error -> t
