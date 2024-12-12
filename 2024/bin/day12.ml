open Core

module Point = struct
  module T = struct
    type t = int * int [@@deriving hash, compare, sexp_of]
  end

  include T
  include Comparator.Make(T)
end

let parse_input =
  let points = Hashtbl.create (module Point)
  and lines = In_channel.read_lines "/Users/chris/code/adventofcode_2023/2024/data/day12.txt"
  in
  List.iteri lines ~f:(fun i l ->
    String.iteri l ~f:(fun j c ->
      Hashtbl.add_exn points ~key:(i, j) ~data:c
    )
  );
  points

let neighbors (i, j) = [(i + 1, j); (i - 1, j); (i, j + 1); (i, j - 1)]

let same table p c = match Hashtbl.find table p with
| Some(v) -> Char.equal v c
| None -> false

let make_regions table =
  let points = Set.of_hashtbl_keys (module Point) table in
  let expand_region set initial =
    let rec inner (acc_p, acc_a) s = function
    | p :: tl ->
      let c = Hashtbl.find_exn table p
      and ns = Set.remove s p in
      let positions = List.filter (neighbors p) ~f:(fun p -> same table p c) in
      inner
        (acc_p + 4 - List.length positions, acc_a + 1)
        (List.fold positions ~init:ns ~f:Set.remove)
        (List.filter positions ~f:(Set.mem ns) @ tl)
    | [] -> ((acc_p, acc_a), s)
    in
    inner (0, 0) set [initial]
  in
  let rec inner acc s = match Set.min_elt s with
  | None -> acc
  | Some(p) -> let r, ns = expand_region s p in inner (r :: acc) ns
in
  inner [] points

let part1 = List.fold ~init:0 ~f:(fun acc (p, a) -> acc + (p * a))

let () =
  let table = parse_input in
  let regions = make_regions table in
  printf "part1=%d\n" (part1 regions)


