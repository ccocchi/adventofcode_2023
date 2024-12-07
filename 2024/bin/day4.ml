open Core

let parse_input =
  In_channel.read_lines "/Users/chris/code/adventofcode_2023/2024/data/day4.txt"
  |> List.map ~f:(fun l -> String.to_array l)
  |> Array.of_list

type dir = N | NE | E | SE | S | SW | W | NW
type point = {
  x: int;
  y: int;
}

let all_directions = [N; NE; E; SE; S; SW; W; NW]

let possibilities (start: point) m n = List.filter_map all_directions ~f:(fun d -> match d with
  | N ->
    if (start.x - 3 >= 0) then
      Some((
        {x = start.x - 1; y = start.y },
        {x = start.x - 2; y = start.y },
        {x = start.x - 3; y = start.y }
      ))
    else
      None
  | NE ->
    if (start.x - 3 >= 0 && start.y + 3 < n) then
      Some((
        {x = start.x - 1; y = start.y + 1 },
        {x = start.x - 2; y = start.y + 2 },
        {x = start.x - 3; y = start.y + 3 }
      ))
    else
      None
  | E ->
    if (start.y + 3 < n) then
      Some((
        {x = start.x; y = start.y + 1 },
        {x = start.x; y = start.y + 2 },
        {x = start.x; y = start.y + 3 }
      ))
    else
      None
  | SE ->
    if (start.x + 3 < m && start.y + 3 < n) then
      Some((
        {x = start.x + 1; y = start.y + 1 },
        {x = start.x + 2; y = start.y + 2 },
        {x = start.x + 3; y = start.y + 3 }
      ))
    else
      None
  | S ->
    if (start.x + 3 < m) then
      Some((
        {x = start.x + 1; y = start.y },
        {x = start.x + 2; y = start.y },
        {x = start.x + 3; y = start.y }
      ))
    else
      None
  | SW ->
    if (start.x + 3 < m && start.y - 3 >= 0) then
      Some((
        {x = start.x + 1; y = start.y - 1 },
        {x = start.x + 2; y = start.y - 2 },
        {x = start.x + 3; y = start.y - 3 }
      ))
    else
      None
  | W ->
    if (start.y - 3 >= 0) then
      Some((
        {x = start.x; y = start.y - 1 },
        {x = start.x; y = start.y - 2 },
        {x = start.x; y = start.y - 3 }
      ))
    else
      None
  | NW ->
    if (start.x - 3 >= 0 && start.y - 3 >= 0) then
      Some((
        {x = start.x - 1; y = start.y - 1 },
        {x = start.x - 2; y = start.y - 2 },
        {x = start.x - 3; y = start.y - 3 }
      ))
    else
      None
)

let test_xmas (mat: char array array) (possibilities: (point * point * point) list) =
  List.count possibilities ~f:(fun (a, b, c) ->
    (Char.equal mat.(a.x).(a.y) 'M') &&
    (Char.equal mat.(b.x).(b.y) 'A') &&
    Char.equal mat.(c.x).(c.y) 'S'
  )

let test_cross (mat: char array array) x y =
try
  let str = String.of_char_list [mat.(x - 1).(y - 1); mat.(x - 1).(y + 1); mat.(x + 1).(y - 1); mat.(x + 1).(y + 1)]
in
  String.equal str "MSMS" || String.equal str "MMSS" || String.equal str "SSMM" || String.equal str "SMSM"
with
| _ -> false


let part1 (mat: char array array) =
  let m, n = (Array.length mat, Array.length mat.(0)) in
  Array.foldi mat ~init:Int.zero ~f:(fun x acc row ->
    acc + Array.foldi row ~init:Int.zero ~f:(fun y acc2 c -> match c with
    | 'X' ->
      let origin = { x = x; y = y } in
      acc2 + test_xmas mat (possibilities origin m n)
    | _ -> acc2
    )
  )

let part2 (mat: char array array) =
  Array.foldi mat ~init:Int.zero ~f:(fun x acc row ->
    acc + Array.foldi row ~init:Int.zero ~f:(fun y acc2 c -> match c with
    | 'A' when (test_cross mat x y) -> acc2 + 1
    | _ -> acc2
    )
  )

let () =
  let matrix = parse_input in
  printf "part1=%d\n" (part1 matrix);
  printf "part2=%d\n" (part2 matrix)
