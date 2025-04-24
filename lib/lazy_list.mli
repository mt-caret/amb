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
val concat_map : 'a t -> f:('a -> 'b t) -> 'b t
val uncons_exn : 'a t -> 'a * 'a t

module Ref : sig
  type 'a list := 'a t
  type 'a t

  val create : 'a list -> 'a t
  val next : 'a t -> 'a option
  val next_exn : 'a t -> 'a
end
