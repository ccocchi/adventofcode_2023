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

let same table p c = match Hashtbl.find table p with
| Some(v) -> Char.equal v c
| None -> false

let neighbors (i, j) = [(i + 1, j); (i - 1, j); (i, j + 1); (i, j - 1)]

let make_regions table =
  let points = Set.of_hashtbl_keys (module Point) table in
  let expand_region set initial =
    let rec inner acc s = function
    | p :: tl ->
      let c = Hashtbl.find_exn table p
      and ns = Set.remove s p in
      let positions = List.filter (neighbors p) ~f:(fun p -> same table p c) in
      inner
        (p :: acc)
        (List.fold positions ~init:ns ~f:Set.remove)
        (List.filter positions ~f:(Set.mem ns) @ tl)
    | [] -> (acc, s)
    in
    inner [] set [initial]
  in
  let rec inner acc s = match Set.min_elt s with
  | None -> acc
  | Some(p) -> let r, ns = expand_region s p in inner (r :: acc) ns
in
  inner [] points

let part1 table regions =
  let perimeter l = List.fold l ~init:0 ~f:(fun acc p ->
    let c = Hashtbl.find_exn table p in
    acc + List.count (neighbors p) ~f:(fun n -> same table n c |> not)
  )
  in
  List.fold regions ~init:0 ~f:(fun acc l -> acc + (perimeter l) * List.length l)

let part2 table regions =
  let count_corners l =
    let corners (i, j) = [
      ((i - 1, j), (i, j + 1), (i - 1, j + 1));
      ((i, j + 1), (i + 1, j), (i + 1, j + 1));
      ((i + 1, j), (i, j - 1), (i + 1, j - 1));
      ((i, j - 1), (i - 1, j), (i - 1, j - 1))
    ]
    in
    List.fold l ~init:0 ~f:(fun acc pos ->
      let c = Hashtbl.find_exn table pos in
      acc + List.fold (corners pos) ~init:0 ~f:(fun acc (p1, p2, p3) ->
        if ((same table p1 c) && (same table p2 c) && not (same table p3 c)) then (* inner corner *)
          acc + 1
        else if ((not (same table p1 c)) && (not (same table p2 c))) then (* outer corner *)
          acc + 1
        else
          acc
      )
    )
  in
  List.fold regions ~init:0 ~f:(fun acc l -> acc + (count_corners l) * List.length l)

let () =
  let table = parse_input in
  let regions = make_regions table in
  printf "part1=%d\n" (part1 table regions);
  printf "part2=%d\n" (part2 table regions)


