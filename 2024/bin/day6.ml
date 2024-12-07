open Core

type dir = N | E | S | W

let rotate x y d = match d with
| N -> (x + 1, y + 1, E)
| E -> (x + 1, y - 1, S)
| S -> (x - 1, y - 1, W)
| W -> (x - 1, y + 1, N)

let continue x y dir = match dir with
| N -> (x - 1, y)
| E -> (x, y + 1)
| S -> (x + 1, y)
| W -> (x, y - 1)

let parse_input =
  In_channel.read_lines "/Users/chris/code/adventofcode_2023/2024/data/day6.txt"
  |> List.map ~f:(fun l -> String.to_array l)
  |> Array.of_list

let move_guard mat (i, j) =
  let rec inner x y dir res =
    try
      match mat.(x).(y) with
      | '.' | '^' ->
        let (nx, ny) = continue x y dir in inner nx ny dir ((x,y) :: res)
      | _ ->
        let (nx, ny, nd) = rotate x y dir in inner nx ny nd res
    with
    | _ -> res
in
  inner i j N []

let _debug mat =
  let n, m = (Array.length mat, Array.length mat.(0)) in
  for j = 0 to n - 1 do
    for i = 0 to m - 1 do
      let i = mat.(j).(i) in
      printf "%c" (if i = 0 then '.' else '#')
    done;
    Out_channel.newline stdout;
  done;
  Out_channel.newline stdout

let cmp (a1, b1) (a2, b2) = match Int.compare a1 a2 with
| 0 -> Int.compare b1 b2
| i -> i

let eq (a1, b1) (a2, b2) = Int.equal a1 a2 && Int.equal b1 b2

let part1 mat =
  move_guard mat (94, 73)
  |> List.sort ~compare:cmp
  |> List.remove_consecutive_duplicates ~equal:eq

let ends_in_loop mat (i, j) =
  let rec inner x y dir res n =
    try
      match mat.(x).(y) with
      | '.' | '^' ->
        if n > 10000 then true
        else
        let (nx, ny) = continue x y dir in inner nx ny dir ((x,y) :: res) (n + 1)
      | _ ->
        let (nx, ny, nd) = rotate x y dir in inner nx ny nd res n
    with
    | _ -> false
in
  inner i j N [] 0

let part2 original (path: (int * int) list) =
  List.filter path ~f:(fun x ->
    let mat = Array.copy_matrix original
    and (i, j) = x
    in
    mat.(i).(j) <- '#';
    ends_in_loop mat (94, 73)
  )
  |> List.length

let () =
  let mat = parse_input in
  let path = part1 mat
in
  printf "part1=%d\n" (List.length path);
  printf "part2=%d\n" (part2 mat path)
