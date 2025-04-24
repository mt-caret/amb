open! Core

val amb : 'a list -> 'a
val fail : unit -> 'a

module type Enumerable = sig
  type t [@@deriving enumerate]
end

val amb' : (module Enumerable with type t = 'a) -> 'a
val collect : f:(unit -> 'a) -> 'a list
val collect_one : f:(unit -> 'a) -> 'a option
val collect_lazy : f:(unit -> 'a) -> 'a Lazy_list.t

module Lazy_list = Lazy_list
