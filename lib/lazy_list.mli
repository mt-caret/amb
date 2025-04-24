open! Core

type 'a t =
  | Nil
  | Cons of 'a * 'a t lazy_t
[@@deriving sexp]

val singleton : 'a -> 'a t
val concat : 'a t t -> 'a t
val of_list : 'a list -> 'a t
val to_list : 'a t -> 'a list
val map : 'a t -> f:('a -> 'b) -> 'b t
val uncons_exn : 'a t -> 'a * 'a t
