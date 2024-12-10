open Core

let parse_input =
  In_channel.read_lines "/Users/chris/code/adventofcode_2023/2024/data/day10.txt"
  |> List.map ~f:(fun l -> String.to_array l |> Array.map ~f:(fun c -> Int.of_string (Char.to_string c)))
  |> Array.of_list

let get mat i j =
  try
    Some((i, j), mat.(i).(j))
  with
  | Invalid_argument(_) -> None

let hike mat start ~init ~f=
  let rec inner acc = function
  | [] -> acc
  | (pos, v) :: xs when v = 9 -> inner (f pos acc) xs
  | ((i, j), v) :: xs ->
    let steps = [get mat (i + 1) j; get mat (i - 1) j; get mat i (j + 1); get mat i (j - 1)] in
    let next = List.filter_map steps ~f:(fun e -> match e with
    | Some(_, n) when n = v + 1 -> e
    | _ -> None
    )
    in
    inner acc (next @ xs)
  in
  inner init [(start, 0)]

let part1 (mat: int array array) trailheads =
  List.map trailheads ~f:(fun x -> hike mat x ~init:[] ~f:(List.cons))
  |> List.map ~f:(List.dedup_and_sort ~compare:(Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare))
  |> List.map ~f:List.length
  |> List.reduce_exn ~f:(+)

let part2 mat trailheads =
  List.map trailheads ~f:(fun x -> hike mat x ~init:0 ~f:(fun _ acc -> acc + 1))
  |> List.reduce_exn ~f:(+)

let () =
  let mat = parse_input in
  let trailheads = Array.foldi mat ~init:[] ~f:(fun i acc ary ->
    let vs = ary |> List.of_array |> List.filter_mapi ~f:(fun j e -> if (e = 0) then Some((i, j)) else None) in
    vs @ acc
  )
  in
  printf("part1=%d\n") (part1 mat trailheads);
  printf("part2=%d\n") (part2 mat trailheads)
