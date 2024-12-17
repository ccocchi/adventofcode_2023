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
  In_channel.read_lines "/Users/chris/code/adventofcode_2023/2024/data/day16.txt"
  |> List.filter_mapi ~f:(fun i s ->
    String.to_list s
    |> List.filter_mapi ~f:(fun j -> function | '#' -> Some((i, j)) | _ -> None)
    |> function | [] -> None | l -> Some(l)
  )
  |> List.concat
  |> Set.of_list (module Coord)

type dir = Up | Down | Left | Right [@@deriving equal]

let neighbors walls (i, j) dir =
  let inner pos d =
    if Set.mem walls pos then None
    else
      let coord = (pos, d)
      and score = if equal_dir d dir then 1 else 1001
      in
      Some((score, coord))
  in
  [
    inner (i - 1, j) Up;
    inner (i + 1, j) Down;
    inner (i, j - 1) Left;
    inner (i, j + 1) Right;
  ]
  |> List.filter_map ~f:(fun x -> x)


let find_path walls start finish =
  let seen = Hashtbl.create (module Coord)
  and openSet = Min_heap.create ~size:64 ~init:((0, 0), Up)
  in
  Min_heap.add openSet (start, Down) ~score:0;
  while Min_heap.length openSet > 0 do
    match Min_heap.pop_min_elt openSet with
    | None -> raise(invalid_arg "no path found")
    | Some((score, (pos, dir))) when Coord.equal pos finish -> begin
      let diff = if equal_dir dir Right then 0 else 1000 in
      match Hashtbl.find seen pos with
      | None -> Hashtbl.add_exn seen ~key:pos ~data:(score + diff)
      | Some(s) when score < s -> Hashtbl.set seen ~key:pos ~data:score
      | _ -> ()
    end
    | Some((score, (pos, dir))) ->
      if not (Hashtbl.mem seen pos) then begin
        neighbors walls pos dir |> List.iter ~f:(fun (ns, x) -> Min_heap.add openSet x ~score:(score + ns));
        Hashtbl.add_exn seen ~key:pos ~data:score
      end
  done;
  Hashtbl.find_exn seen finish

let () =
  (* let heap = Aoc_2024.Min_heap.create ~size:64 ~init:0 in
  let rec inner h = match (Aoc_2024.Min_heap.pop_min_elt h) with
  | None -> ()
  | Some((_, e)) -> printf "%d\n" e;
    inner h
  in
  List.iter [1;2;3;17;19;36;7;25;100] ~f:(fun x -> Aoc_2024.Min_heap.add heap x ~score: x);
  inner heap *)
  let walls = parse_input in
  printf "%d\n" (find_path walls (1, 139) (139, 1))




