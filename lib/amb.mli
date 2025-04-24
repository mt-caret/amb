open! Core

val amb : 'a Lazy_list.t -> 'a
val fail : unit -> 'a
val amb' : 'a list -> 'a

module type Enumerable = sig
  type t [@@deriving enumerate]
end

val amb_of_enum : (module Enumerable with type t = 'a) -> 'a
val collect : f:(unit -> 'a) -> 'a list
val collect_one : f:(unit -> 'a) -> 'a option
val collect_lazy : f:(unit -> 'a) -> 'a Lazy_list.t

module Lazy_list = Lazy_list
