open! Core

type 'a Effect.t += Choose : 'a Lazy_list.t -> 'a Effect.t

let amb elements = Effect.perform (Choose elements)
let fail () = amb Nil
let amb' list = amb (Lazy_list.of_list list)

module type Enumerable = sig
  type t [@@deriving enumerate]
end

let amb_of_enum (type a) (module M : Enumerable with type t = a) = amb' M.all

let collect ~f =
  try [ f () ] with
  | effect Choose elements, k ->
    let r = Multicont.Deep.promote k in
    Lazy_list.map elements ~f:(fun x -> Multicont.Deep.resume r x)
    |> Lazy_list.to_list
    |> List.concat
;;

let collect_one ~f =
  try Some (f ()) with
  | effect Choose elements, k ->
    let r = Multicont.Deep.promote k in
    let rec go = function
      | Lazy_list.Nil -> None
      | Cons (x, xs) ->
        (match Multicont.Deep.resume r x with
         | Some v -> Some v
         | None -> go (force xs))
    in
    go elements
;;

let collect_lazy (type a) ~(f : unit -> a) : a Lazy_list.t =
  try
    let x = f () in
    Lazy_list.singleton x
  with
  | effect Choose elements, k ->
    let r : (_, a Lazy_list.t) Multicont.Deep.resumption = Multicont.Deep.promote k in
    Lazy_list.concat_map elements ~f:(fun x -> Multicont.Deep.resume r x)
;;

module Lazy_list = Lazy_list
