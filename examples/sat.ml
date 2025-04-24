open! Core
open! Amb

module Formula = struct
  type 'a t =
    | Lit of 'a
    | Not of 'a t
    | And of 'a t * 'a t
    | Or of 'a t * 'a t

  let lit x = Lit x
  let not x = Not x
  let ( && ) x y = And (x, y)
  let ( || ) x y = Or (x, y)

  let literals =
    let rec go t accum =
      match t with
      | Lit x -> x :: accum
      | Not t -> go t accum
      | And (t1, t2) | Or (t1, t2) -> go t1 (go t2 accum)
    in
    fun t ~compare -> go t [] |> List.dedup_and_sort ~compare
  ;;
end

let satisfy
      (type a)
      (module M : Comparable.S_plain with type t = a)
      (formula : a Formula.t)
  =
  let assignment =
    Formula.literals formula ~compare:M.compare
    |> List.map ~f:(fun literal -> literal, amb' [ true; false ])
    |> M.Map.of_alist_exn
  in
  let rec go (f : _ Formula.t) =
    match f with
    | Lit literal -> Map.find_exn assignment literal
    | Not f -> not (go f)
    | And (f1, f2) -> go f1 && go f2
    | Or (f1, f2) -> go f1 || go f2
  in
  match go formula with
  | true -> Map.to_alist assignment
  | false -> fail ()
;;

let%expect_test "sat" =
  let f () = satisfy (module String) Formula.((lit "a" || lit "b") && not (lit "c")) in
  collect_one ~f |> [%sexp_of: (string * bool) list option] |> print_s;
  [%expect {| (((a true) (b true) (c false))) |}];
  collect ~f
  |> List.iter ~f:(fun assignment ->
    [%sexp_of: (string * bool) list] assignment |> print_s);
  [%expect
    {|
    ((a true) (b true) (c false))
    ((a true) (b false) (c false))
    ((a false) (b true) (c false))
    |}]
;;
