open Core

let parse_input =
  In_channel.read_lines "/Users/chris/code/adventofcode_2023/2024/data/day2.txt"
  |> List.map ~f: (fun x -> (String.split ~on:' ' x) |> List.map ~f: int_of_string)

type direction = Asc | Desc

let isBounded a b = let abs = Int.abs (a - b) in abs >= 1 && abs <= 3

let isMonotonic a b dir = match dir with
| Asc -> a < b
| Desc -> a > b

let direction a b = if (a < b) then Some(Asc) else Some(Desc)

let isMonotonicAndBounded (level: int list): bool =
  let rec inner l dir = match l with
  | [] -> true
  | _ :: [] -> true
  | a :: b :: tail -> match dir with
    | None -> (isBounded a b) && inner (b :: tail) (direction a b)
    | Some(d) -> (isBounded a b) && (isMonotonic a b d) && inner (b :: tail) dir
in
  inner level None

let isDampened (level: int list) =
  let l = List.findi ~f:(fun i _ -> isMonotonicAndBounded (List.filteri ~f:(fun j _ -> not (Int.equal i j)) level)) level
  in
  match l with
  | Some(_) -> true
  | None -> false

let part1 reports = List.count ~f:isMonotonicAndBounded reports

let part2 reports = List.count ~f:(fun x -> (isMonotonicAndBounded x) || isDampened x) reports

let () =
  let reports = parse_input
in
  printf "part1=%d\n" (part1 reports);
  printf "part2=%d\n" (part2 reports)
