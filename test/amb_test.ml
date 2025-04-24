open! Core
open! Amb

let f () =
  let x = amb' [ 1; 2; 3 ] in
  let y = amb' [ 4; 5; 6; 7; 8 ] in
  if x * y <> 8 then fail ();
  x, y
;;

let%expect_test "collect" =
  collect ~f |> [%sexp_of: (int * int) list] |> print_s;
  [%expect {| ((1 8) (2 4)) |}]
;;

let%expect_test "collect_one" =
  collect_one ~f |> [%sexp_of: (int * int) option] |> print_s;
  [%expect {| ((1 8)) |}]
;;

let%expect_test "collect_lazy" =
  let result = collect_lazy ~f |> Lazy_list.Ref.create in
  let print_next () =
    [%sexp_of: (int * int) option] (Lazy_list.Ref.next result) |> print_s
  in
  print_next ();
  [%expect {| ((1 8)) |}];
  print_next ();
  [%expect {| ((2 4)) |}];
  print_next ();
  [%expect {| () |}]
;;
