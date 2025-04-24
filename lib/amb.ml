open! Core

type 'a Effect.t += Choose : 'a list -> 'a Effect.t

let amb elements = Effect.perform (Choose elements)
let fail () = amb []

module type Enumerable = sig
  type t [@@deriving enumerate]
end

let amb' (type a) (module M : Enumerable with type t = a) = amb M.all

let collect ~f =
  try [ f () ] with
  | effect Choose elements, k ->
    let r = Multicont.Deep.promote k in
    List.concat_map elements ~f:(fun x -> Multicont.Deep.resume r x)
;;

let collect_one ~f =
  try Some (f ()) with
  | effect Choose elements, k ->
    let r = Multicont.Deep.promote k in
    let rec go = function
      | [] -> None
      | x :: xs ->
        (match Multicont.Deep.resume r x with
         | Some v -> Some v
         | None -> go xs)
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
    Lazy_list.of_list elements
    |> Lazy_list.map ~f:(fun x -> Multicont.Deep.resume r x)
    |> Lazy_list.concat
;;

module Lazy_list = Lazy_list
