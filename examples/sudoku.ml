open! Core
open! Amb

let solve problem =
  let numbers = List.range 1 10 in
  let numbers_set = Int.Set.of_list numbers in
  let problem =
    List.mapi problem ~f:(fun y line ->
      let line = List.mapi line ~f:(fun x cell -> x, cell) |> Int.Map.of_alist_exn in
      y, line)
    |> Int.Map.of_alist_exn
  in
  let update problem y x n =
    Map.update problem y ~f:(fun line ->
      Map.set (Option.value_exn line) ~key:x ~data:(Some n))
  in
  let empty_cells =
    Map.to_alist problem
    |> List.concat_map ~f:(fun (y, line) ->
      Map.to_alist line
      |> List.filter_map ~f:(fun (x, cell) ->
        if Option.is_none cell then Some (x, y) else None))
  in
  List.fold empty_cells ~init:problem ~f:(fun problem (x, y) ->
    let horizontal_nums =
      Map.find_exn problem y |> Map.data |> List.filter_opt |> Int.Set.of_list
    in
    let vertical_nums =
      Map.map problem ~f:(fun line -> Map.find_exn line x)
      |> Map.data
      |> List.filter_opt
      |> Int.Set.of_list
    in
    let square_nums =
      let indices index =
        let start = index / 3 * 3 in
        List.range start (start + 3)
      in
      List.cartesian_product (indices x) (indices y)
      |> List.map ~f:(fun (x, y) -> Map.find_exn (Map.find_exn problem y) x)
      |> List.filter_opt
      |> Int.Set.of_list
    in
    let possible_numbers =
      Set.diff
        numbers_set
        (Int.Set.union_list [ horizontal_nums; vertical_nums; square_nums ])
      |> Set.to_list
    in
    let number = amb' possible_numbers in
    update problem y x number)
  |> Map.map ~f:(fun line ->
    Map.map line ~f:(fun cell -> Option.value_exn cell) |> Map.data)
  |> Map.data
;;

let parse_problem problem =
  String.strip problem
  |> String.split_lines
  |> List.map ~f:(fun line ->
    String.to_list line |> List.map ~f:(fun ch -> Char.to_string ch |> Int.of_string_opt))
;;

let print_solution solution =
  List.map solution ~f:(fun line -> List.map line ~f:Int.to_string |> String.concat)
  |> String.concat ~sep:"\n"
  |> print_endline
;;

let%expect_test "sudoku" =
  let problem =
    parse_problem
      {|
53..7....
6..195...
.98....6.
8...6...3
4..8.3..1
7...2...6
.6....28.
...419..5
....8..79|}
  in
  collect_one ~f:(fun () -> solve problem) |> Option.value_exn |> print_solution;
  [%expect
    {|
    534678912
    672195348
    198342567
    859761423
    426853791
    713924856
    961537284
    287419635
    345286179
    |}]
;;
