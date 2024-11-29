open Core

let int_of_char ch = Char.(to_int ch - to_int '0')

let read_lines =
  In_channel.read_lines "/Users/chris/code/adventofcode_2023/data/day17.txt"
  |> List.map ~f:(fun l -> l |>  String.to_list |> List.map ~f:int_of_char |> Array.of_list)
  |> Array.of_list

module Vertex = struct
  type t = int * int * int * int * int [@@deriving sexp, ord, hash]
end

let compare_v_cost (v1, c1) (v2, c2) =
  if (c1 = c2) then Vertex.compare v1 v2 else Int.compare c1 c2

let _debug mat =
  let n, m = (Array.length mat, Array.length mat.(0)) in
  for j = 0 to n - 1 do
    for i = 0 to m - 1 do
      printf "%i" mat.(j).(i)
    done;
    Out_channel.newline stdout;
  done;
  Out_channel.newline stdout

let solve mat min max =
  let m, n = (Array.length mat, Array.length mat.(0)) in
  let heap = Pairing_heap.create ~min_size:(m * n * 2) ~cmp:compare_v_cost ()
  and states = Hashtbl.create (module Vertex) in
  let move (i, j, di, dj, dist) cost =
    let x = i + di and y = j + dj in
    if (x >= 0 && x < m && y >= 0 && y < n) then begin
      let c = mat.(x).(y)
      and v = (x, y, di, dj, dist) in
      match Hashtbl.add states ~key:v ~data:(c + cost) with
      | `Ok -> Pairing_heap.add heap (v, c + cost)
      | _ -> ()
    end
  in
  let rec inner () =
    let e = Pairing_heap.pop_exn heap in
    match e with
  | ((i, j, _, _, dist), c) when i = (m - 1) && j = (n - 1) && dist >= min -> c
  | ((i, j, di, dj, dist), c) ->
    if (dist >= min) then begin
      move (i, j, dj, -di, 1) c;
      move (i, j, -dj, di, 1) c
    end;
    if (dist < max) then move (i, j, di, dj, dist + 1) c;
    inner ()
  in
  move (0, 0, 1, 0, 1) 0;
  move (0, 0, 0, 1, 1) 0;
  inner ()

let () =
  let mat = read_lines in
  printf "part1=%d\n" (solve mat 0 3);
  printf "part2=%d\n" (solve mat 4 10)
