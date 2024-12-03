open Core

let partition l =
  let rec inner list l1 l2 = match list with
  | hd :: tail ->
    let a = int_of_string (List.hd_exn hd)
    and b = int_of_string (List.last_exn hd)
    in
      inner tail (a :: l1) (b :: l2)
  | [] -> (l1, l2)
in
  inner l [] []

let read_lines =
  In_channel.read_lines "/Users/chris/code/adventofcode_2023/2024/data/day1.txt"
  |> List.map ~f: (String.split ~on:' ')
  |> partition
  |> Tuple2.map ~f:(List.sort ~compare:Int.compare)

let part1 lists =
  List.fold2_exn (Tuple2.get1 lists) (Tuple2.get2 lists) ~init:Int.zero ~f:(fun acc a b -> acc + Int.abs (a - b))

let part2 lists =
  let a = (Tuple2.get1 lists)
  and b = (Tuple2.get2 lists)
in
  List.fold a ~init:Int.zero ~f:(fun acc e -> acc + e * List.count b ~f:(fun x -> Int.equal x e))


let () =
  let l = read_lines
in
  printf "part1=%d\n" (part1 l);
  printf "part2=%d\n" (part2 l)
