open Core
open Aoc_2024

module Coord = struct
  module T = struct
    type t = int * int [@@deriving equal, hash, compare, sexp_of]
  end

  include T
  include Comparator.Make(T)

  let _print (i, j) = printf "%d, %d\n" i j

  let cardinal_distance (i1, j1) (i2, j2) = Int.abs (i1 - i2) + Int.abs (j1 - j2)
end

let parse_input =
  In_channel.read_lines "/Users/chris/code/adventofcode_2023/2024/data/day20.txt"
  |> List.filter_mapi ~f:(fun i s ->
    String.to_list s
    |> List.filter_mapi ~f:(fun j -> function | '#' -> Some((i, j)) | _ -> None)
    |> function | [] -> None | l -> Some(l)
  )
  |> List.concat
  |> Set.of_list (module Coord)

type step = {
  pos: Coord.t;
  path: Coord.t list
}

let find_path walls start finish =
  let seen = Hashtbl.create (module Coord)
  and openSet = Min_heap.create ~size:64 ~init:{ pos = start; path = [] }
  and neighbors (i, j) =
    [(i - 1, j); (i + 1, j); (i, j - 1); (i, j + 1);]
    |> List.filter ~f:(fun (i, j) -> i >= 0 && i < 140 && j >= 0 && j < 140)
    |> List.filter ~f:(fun p -> not (Set.mem walls p))
  and res = ref []
  in
  Min_heap.add openSet { pos = start; path = [] } ~score:0;
  while Min_heap.length openSet > 0 do
    match Min_heap.pop_min_elt openSet with
    | None -> raise(invalid_arg "no path found")
    | Some((score, { pos; path })) when Coord.equal pos finish -> begin
      Hashtbl.add_exn seen ~key:pos ~data:(score);
      res := (pos :: path);
      Min_heap.empty openSet
    end
    | Some((score, { pos; path })) ->
      if not (Hashtbl.mem seen pos) then begin
        Hashtbl.add_exn seen ~key:pos ~data:score;
        List.iter (neighbors pos) ~f:(fun p ->
          Min_heap.add openSet { pos = p; path = pos :: path } ~score:(score + 1)
        )
      end
  done;
  List.rev !res

(* let removable_walls walls m n =
  let can_go_through p1 p2 =
    let (i1, j1) = p1 and (i2, j2) = p2 in
    i1 >= 0 && i1 < m && i2 >= 0 && i2 < m &&
    j1 >= 0 && j1 < n && j2 >= 0 && j2 < n &&
    not (Set.mem walls p1) && not (Set.mem walls p2)
  in
  List.filter (Set.to_list walls) ~f:(fun (i, j) ->
    (can_go_through (i - 1, j) (i + 1, j)) || (can_go_through (i, j - 1) (i, j + 1))
  ) *)


let find_shortcuts path ~f =
  let rec inner acc = function
  | [] -> acc
  | x :: xs ->
    match List.filteri xs ~f:(fun i p -> let d = Coord.cardinal_distance p x in (i - d + 1) >= 100 && f(d)) with
    | [] -> inner acc xs
    | l -> inner ((List.map l ~f:(fun p -> (x, p))) @ acc) xs
  in
  inner [] path

let () =
  let walls = parse_input
  and start = (139, 87) (* (3, 1) *)
  and finish = (119, 79) (* (7, 5) *)
  in
  let path = find_path walls start finish
  in
  let l = find_shortcuts path ~f:(fun d -> d = 2) in
  printf "part1=%d\n" (List.length l);

  let l = find_shortcuts path ~f:(fun d -> d <= 20) in
  printf "part2=%d\n" (List.length l)
  (* l
  |> List.rev
  |> List.iter ~f:(fun ((i1, j1), (i2, j2)) ->
    printf "(%d, %d) -> (%d, %d)\n" i1 j1 i2 j2
  ) *)


  (* print_endline "path:";
  List.iter path ~f:Coord._print;
  printf "size=%d\n" ((List.length path) - 1);
  let l = find_shortcuts path in
  printf "shortcuts=%d\n" ((List.length l));
  l
  |> List.rev
  |> List.iter ~f:(fun ((i1, j1), (i2, j2)) ->
    printf "(%d, %d) -> (%d, %d)\n" i1 j1 i2 j2
  ) *)

  (* 1013106 *)

  (* printf "pico=%d\n" score;
  printf "cheats=%d\n" (List.length cheats);
  let cnt = List.count cheats ~f:(fun p ->
    let ns = find_path (Set.remove walls p) start finish in
    (score - ns) >= 100
  )
  in
  printf "part1=%d\n" cnt *)
