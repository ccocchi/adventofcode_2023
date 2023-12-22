open Core

module Beam = struct
  type direction = Up | Down | Right | Left
  type t = (int * int) * direction

  let right (((i, j), _): t): t = ((i, j + 1), Right)
  let left (((i, j), _): t): t = ((i, j - 1), Left)
  let up (((i, j), _): t): t = ((i - 1, j), Up)
  let down (((i, j), _): t): t = ((i + 1, j), Down)

  let pos (b: t) = fst b

  let next (beam: t) (c: char): t list =
    match (c, (snd beam)) with
  | ('.', Up) -> [ up beam ]
  | ('.', Down) -> [ down beam ]
  | ('.', Left) -> [ left beam ]
  | ('.', Right) -> [ right beam ]
  | ('/', Up) -> [ right beam ]
  | ('/', Down) -> [ left beam ]
  | ('/', Left) -> [ down beam ]
  | ('/', Right) -> [ up beam ]
  | ('\\', Up) -> [ left beam ]
  | ('\\', Down) -> [ right beam ]
  | ('\\', Left) -> [ up beam ]
  | ('\\', Right) -> [ down beam ]
  | ('|', Up) -> [ up beam ]
  | ('|', Down) -> [ down beam ]
  | ('|', _) -> [ up beam ; down beam ]
  | ('-', Left) -> [ left beam ]
  | ('-', Right) -> [ right beam ]
  | ('-', _) -> [ left beam ; right beam ]
  | _ -> []
end

let read_lines =
  In_channel.read_lines "/Users/chris/code/adventofcode_2023/data/day16.txt"
  |> List.map ~f:String.to_array
  |> Array.of_list

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

let energize mat start =
  let m, n = (Array.length mat, Array.length mat.(0)) in
  let emat = Array.make_matrix ~dimx:m ~dimy:n 0 in
  let rec inner (beams: Beam.t list) = match beams with
  | ((i, _), _) :: tail when i < 0 || i = m -> inner tail
  | ((_, j), _) :: tail when j < 0 || j = n -> inner tail
  | b :: tail -> (
    let i, j = Beam.pos b in
    let c = mat.(i).(j) in
    match c with
      | '|' when emat.(i).(j) = 1 -> inner tail
      | '-' when emat.(i).(j) = 1 -> inner tail
      | _ ->
        emat.(i).(j) <- 1;
        inner (List.append (Beam.next b c) tail)
    )
  | [] -> ()
in
  inner [start];
  Array.fold emat ~init:0 ~f:(fun acc l -> acc + (Array.fold l ~init:0 ~f:(+) ))

let part2 mat =
  let m, n = (Array.length mat, Array.length mat.(0)) in
  let l1 = List.init (m - 1) ~f:(fun x -> ((x, 0), Beam.Right))
  and l2 = List.init (m - 1) ~f:(fun x -> ((x, n - 1), Beam.Left))
  and l3 = List.init (n - 1) ~f:(fun x -> ((0, x), Beam.Down))
  and l4 = List.init (n - 1) ~f:(fun x -> ((m - 1, x), Beam.Up))
  in
  l1 @ l2 @ l3 @ l4
  |> List.fold ~init:0 ~f:(fun acc b ->
    let n = energize mat b in
    if (n > acc) then n else acc
  )

let () =
  let mat = read_lines in
  printf "part1=%d\n" (energize mat ((0, 0), Right));
  printf "part2=%d\n" (part2 mat)
