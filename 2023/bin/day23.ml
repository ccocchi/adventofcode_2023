open Core

module Dir = struct
  type t = N | S | E | W [@@deriving eq]

  let opposite = function
  | N -> S
  | S -> N
  | E -> W
  | W -> E

  let _to_char = function
  | N -> 'N'
  | S -> 'S'
  | E -> 'E'
  | W -> 'W'
end

module Position = struct
  module T = struct
    type t = int * int [@@deriving ord, sexp]
  end

  include T
  include Comparator.Make(T)
end

let read_lines =
  In_channel.read_lines "/Users/chris/code/adventofcode_2023/data/day23.txt"
  |> List.map ~f:String.to_array
  |> Array.of_list

let find_bound = Array.find_mapi_exn ~f:(fun i c -> if Char.equal '.' c then Some(i) else None)

let next_steps (i, j, cost, dir) =
  [(i, j + 1, cost + 1, Dir.E); (i, j - 1, cost + 1, Dir.W); (i - 1, j, cost + 1, Dir.N); (i + 1, j, cost + 1, Dir.S)]
  |> List.filter ~f:(fun (_, _, _, d) -> Dir.equal d (Dir.opposite dir) |> not)

let _next_steps2 (i, j, cost, dir, visited) =
  let set = Set.add visited (i, j) in
  [
    (i, j + 1, cost + 1, Dir.E, set);
    (i, j - 1, cost + 1, Dir.W, set);
    (i - 1, j, cost + 1, Dir.N, set);
    (i + 1, j, cost + 1, Dir.S, set)
  ]
  |> List.filter ~f:(fun (_, _, _, d, _) -> Dir.equal d (Dir.opposite dir) |> not)

let part1 mat =
  let m = Array.length mat in
  let ie, je = (m - 1, find_bound mat.(m - 1)) in
  let rec inner res steps = match steps with
  | (i, j, cost, _) :: tail when i = ie && j = je -> inner (cost + 1 :: res) tail
  | [] -> List.max_elt res ~compare:Int.compare |> Option.value_exn
  | step :: tail ->
    let i, j, cost, dir = step in
    match mat.(i).(j) with
    | '#' -> inner res tail
    | '>' ->
      if Dir.equal dir Dir.W then inner res tail
      else inner res ((i, j + 1, cost + 1, Dir.E) :: tail)
    | '<' ->
      if Dir.equal dir Dir.E then inner res tail
      else inner res ((i, j - 1, cost + 1, Dir.W) :: tail)
    | 'v' ->
      if Dir.equal dir Dir.N then inner res tail
      else inner res ((i + 1, j, cost + 1, Dir.S) :: tail)
    | '^' ->
      if Dir.equal dir Dir.S then inner res tail
      else inner res ((i - 1, j, cost + 1, Dir.N) :: tail)
    | _ ->
      inner res ((next_steps step) @ tail)
  in
  inner [] [(0, find_bound mat.(0), -1, S)]

let _part2 _ = 6442 (* TODO *)

let () =
  let mat = read_lines in
  printf "part1=%d\n" (part1 mat);
  printf "part2=%d\n" (_part2 mat)

