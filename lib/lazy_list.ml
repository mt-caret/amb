open! Core

type 'a t =
  | Nil
  | Cons of 'a * 'a t lazy_t

let rec of_list : 'a list -> 'a t = function
  | [] -> Nil
  | x :: xs -> Cons (x, lazy (of_list xs))
;;

let[@tail_mod_cons] rec to_list : 'a t -> 'a list = function
  | Nil -> []
  | Cons (x, xs) -> x :: to_list (force xs)
;;

let sexp_of_t sexp_of_a t = [%sexp_of: a list] (to_list t)
let t_of_sexp sexp_of_a sexp = of_list (List.t_of_sexp sexp_of_a sexp)
let singleton x = Cons (x, lazy Nil)

let rec concat : 'a t t -> 'a t = function
  | Nil -> Nil
  | Cons (Nil, xs) -> concat (force xs)
  | Cons (Cons (x, xs), ys) -> Cons (x, lazy (concat (Cons (force xs, ys))))
;;

let rec map : 'a t -> f:('a -> 'b) -> 'b t =
  fun t ~f ->
  match t with
  | Nil -> Nil
  | Cons (x, xs) -> Cons (f x, lazy (map (force xs) ~f))
;;

let concat_map t ~f = concat (map t ~f)

let uncons_exn = function
  | Nil -> failwith "uncons_exn: empty list"
  | Cons (x, xs) -> x, force xs
;;

module Ref = struct
  type nonrec 'a t = 'a t ref

  let create t = ref t

  let next t =
    match !t with
    | Nil -> None
    | Cons (x, xs) ->
      t := force xs;
      Some x
  ;;

  let next_exn t =
    match next t with
    | None -> failwith "next_exn: no more elements"
    | Some x -> x
  ;;
end
