open Core
open Aoc_2024

module Coord = struct
  module T = struct
    type t = int * int [@@deriving equal, hash, compare, sexp_of]
  end

  include T
  include Comparator.Make(T)

  let _print (i, j) = printf "%d, %d\n" i j
end

let parse_input =
  In_channel.read_lines "/Users/chris/code/adventofcode_2023/2024/data/day18.txt"
  |> List.map ~f:(String.split ~on:',')
  |> List.map ~f:(function
    | x :: y :: _ -> (int_of_string y, int_of_string x)
    | _ -> invalid_arg("bad input")
  )

type step = {
  pos: Coord.t;
  path: Coord.t list
}

let find_path walls start finish =
  let seen = Hashtbl.create (module Coord)
  and openSet = Min_heap.create ~size:64 ~init:{ pos = (0, 0); path = [] }
  and neighbors (i, j) =
    [(i - 1, j); (i + 1, j); (i, j - 1); (i, j + 1);]
    |> List.filter ~f:(fun (i, j) -> i >= 0 && i < 71 && j >= 0 && j < 71)
    |> List.filter ~f:(fun p -> not (Set.mem walls p))
  in
  Min_heap.add openSet { pos = start; path = [start] } ~score:0;
  while Min_heap.length openSet > 0 do
    match Min_heap.pop_min_elt openSet with
    | None -> raise(invalid_arg "no path found")
    | Some((score, { pos; _ })) when Coord.equal pos finish -> begin
      Hashtbl.add_exn seen ~key:pos ~data:(score);
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
  Hashtbl.find_exn seen finish

let part2 l set =
  let _ = List.fold_until l ~init:set ~finish:(fun s -> Set.min_elt_exn s) ~f:(fun acc p ->
    try
      let s = (Set.add acc p) in
      let _ = find_path s (0, 0) (70, 70) in
      Continue(s)
    with
    | _ ->
      printf "part2 stopped at %d,%d\n"(snd p) (fst p) ;
      Stop(p)
  ) in ()

let () =
  let walls = parse_input in
  let set = Set.of_list (module Coord) (List.take walls 2048) in
  let n = find_path set (0, 0) (70, 70) in
  printf "part1=%d\n" n;
  part2 (List.drop walls 1024) set
