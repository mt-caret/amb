open! Core
open! Amb

let n_queens () =
  List.range 0 8
  |> List.fold ~init:[] ~f:(fun queen_coordinates row ->
    let col = amb' (List.range 0 8) in
    List.iter queen_coordinates ~f:(fun (r, c) ->
      if c = col || r - c = row - col || r + c = row + col then fail ());
    (row, col) :: queen_coordinates)
  |> List.rev
;;

let board_to_string queen_coordinates =
  List.init 8 ~f:(fun row ->
    List.init 8 ~f:(fun col ->
      if List.mem queen_coordinates ~equal:[%equal: int * int] (row, col)
      then 'Q'
      else '_')
    |> String.of_char_list)
  |> String.concat ~sep:"\n"
;;

let%expect_test "n_queens" =
  let result = collect_lazy ~f:n_queens |> Lazy_list.Ref.create in
  let print_one () =
    Lazy_list.Ref.next result
    |> Option.iter ~f:(fun board -> board_to_string board |> print_endline)
  in
  print_one ();
  [%expect
    {|
    Q_______
    ____Q___
    _______Q
    _____Q__
    __Q_____
    ______Q_
    _Q______
    ___Q____
    |}];
  print_one ();
  [%expect {|
    Q_______
    _____Q__
    _______Q
    __Q_____
    ______Q_
    ___Q____
    _Q______
    ____Q___
    |}]
;;
