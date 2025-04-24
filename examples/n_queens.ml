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
  collect_one ~f:n_queens |> Option.value_exn |> board_to_string |> print_endline;
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
    |}]
;;
