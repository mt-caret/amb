open! Core
open! Amb

let f () =
  let x = amb [ 1; 2; 3 ] in
  let y = amb [ 4; 5; 6; 7; 8 ] in
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
  let x, xs = collect_lazy ~f |> Lazy_list.uncons_exn in
  [%sexp_of: int * int] x |> print_s;
  [%expect {| (1 8) |}];
  let x, xs = Lazy_list.uncons_exn xs in
  [%sexp_of: int * int] x |> print_s;
  [%expect {| (2 4) |}];
  print_s [%sexp (xs : (int * int) Lazy_list.t)];
  [%expect {| () |}]
;;
