open Core

let _distance (x1, y1) (x2, y2) =
  let x = Float.square (float_of_int (x2 - x1))
  and y = Float.square (float_of_int (y2 - y1))
in
  Float.sqrt (Float.(+) x y)

let parse_input =
  let table = Hashtbl.create (module Char) in
  (
  In_channel.read_lines "/Users/chris/code/adventofcode_2023/2024/data/day8.txt"
  |> List.iteri ~f:(fun i ->
      String.iteri ~f:(fun j -> function
      | '.' -> ()
      | c ->
        match Hashtbl.find table c with
        | None -> Hashtbl.add_exn table ~key:c ~data:[(i, j)]
        | Some(l) -> Hashtbl.set table ~key:c ~data:((i, j) :: l)
      )
    )
  );
  Hashtbl.data table

let in_bounds (x, y) = x >= 0 && x <= 49 && y >= 0 && y <= 49

let half_antinodes a b =
  let trish (x1, y1) (x2, y2) = (x1 - (x2 - x1), y1 - (y2 - y1)) in [trish a b; trish b a]

let antinodes a b =
  let trish (x1, y1) (x2, y2) =
    let dx = x2 - x1 and dy = y2 - y1 in
    let rec inner acc n =
      let point = (x1 - n * dx, y1 - n * dy) in
      if (in_bounds point) then
        inner (point :: acc) (n + 1)
      else
        acc
  in
    inner [] 0
in
  trish a b @ trish b a

let resolve ~f (list: (int * int) list) =
  let rec inner acc = function
  | [] -> acc
  | x :: xs -> inner (List.concat (List.map xs ~f:(fun e -> f x e)) @ acc) xs
in
  inner [] list

let part1 lists =
  List.map lists ~f:(resolve ~f:half_antinodes)
  |> List.concat
  |> List.filter ~f:in_bounds
  |> List.dedup_and_sort ~compare:(Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare)
  |> List.length

let part2 lists =
  List.map lists ~f:(resolve ~f:antinodes)
  |> List.concat
  |> List.dedup_and_sort ~compare:(Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare)
  |> List.length

let () =
  let lists = parse_input in
  printf "part1=%d\n" (part1 lists);
  printf "part2=%d\n" (part2 lists)
