open Core

module Coord = struct
  module T = struct
    type t = int * int [@@deriving equal, hash, compare, sexp_of]
  end

  include T
  include Comparator.Make(T)
end

type dir = Up | Down | Left | Right

let parse_map =
  let table = Hashtbl.create (module Coord)
  and lines = In_channel.read_lines "/Users/chris/code/adventofcode_2023/2024/data/day15_1.txt"
in
  let set = List.foldi lines ~init:(Set.empty (module Coord)) ~f:(fun i acc l ->
    String.foldi ~init:acc l ~f:(fun j acc2 c ->
      if (Char.equal c 'O') then Hashtbl.set table ~key:(i, j) ~data:1;
      if (Char.equal c '#') then Set.add acc2 (i, j) else acc2
    )
  )
  in
  (set, Set.of_hashtbl_keys (module Coord) table)

let parse_directions =
  In_channel.read_lines "/Users/chris/code/adventofcode_2023/2024/data/day15_2.txt"
  |> List.map ~f:(fun s -> String.to_list s |> List.map ~f:(function
    | '^' -> Up
    | '<' -> Left
    | '>' -> Right
    | 'v' -> Down
    | _ -> raise(Invalid_argument("bad input"))
    )
  )
  |> List.concat

let next (i, j) = function
| Up -> (i - 1, j)
| Down -> (i + 1, j)
| Left -> (i, j - 1)
| Right -> (i, j + 1)

let move walls initial directions =
  let boxes_to_move bs first d =
    let rec inner pos =
      let np = next pos d in
      if Set.mem bs np then inner np
      else if Set.mem walls np then None
      else Some(first, np)
    in
    inner first
  in
  let rec inner pos boxes = function
  | [] -> boxes
  | d :: ds ->
    let np = next pos d in
    if Set.mem boxes np then (* try to push boxes *)
      match boxes_to_move boxes np d with
      | None -> inner pos boxes ds (* push in a wall *)
      | Some((first, last)) ->
        inner np (Set.add (Set.remove boxes first) last) ds (* move robot along with boxes *)
    else if Set.mem walls np then inner pos boxes ds (* hit a wall *)
    else inner np boxes ds (* move robot *)
  in
  inner (24, 24) initial directions

let score = List.fold ~init:Int.zero ~f:(fun acc (i, j) -> acc + 100 * i + j)

let part1 =
  let (walls, boxes) = parse_map and dirs = parse_directions in
    move walls boxes dirs
    |> Set.to_list
    |> score

type bracket = Open | Closed

let pair (i, j) = function
| Open -> (i, j + 1)
| Closed -> (i, j - 1)

let parse_map2 =
  let table = Hashtbl.create (module Coord)
  and lines = In_channel.read_lines "/Users/chris/code/adventofcode_2023/2024/data/day15_1.txt"
in
  let set = List.foldi lines ~init:(Set.empty (module Coord)) ~f:(fun i acc l ->
    String.foldi ~init:acc l ~f:(fun j acc2 c ->
      if (Char.equal c 'O') then (
        Hashtbl.set table ~key:(i, j * 2) ~data:Open;
        Hashtbl.set table ~key:(i, j * 2 + 1) ~data:Closed
      );
      if (Char.equal c '#') then Set.add (Set.add acc2 (i, j * 2)) (i, j * 2 + 1)
      else acc2
    )
  )
  in
  (set, table)

let _print walls table (x, y) =
  for i = 0 to 9 do
    for j = 0 to 19 do
      let pos = (i, j) in
      if i = x && j = y then print_string "@"
      else if Set.mem walls pos then print_string "#"
      else match Hashtbl.find table pos with
      | None -> print_string "."
      | Some(bkt) -> match bkt with
        | Open -> print_string "["
        | Closed -> print_string "]"
    done;
    print_endline ""
  done

let _print_dir = function
| Up -> "dir: ^\n"
| Left -> "dir: <\n"
| Right -> "dir: >\n"
| Down -> "dir: v\n"

let _move2 walls table directions =
  let boxes_to_move first d =
    let rec inner acc = function
    | [] -> Some(acc)
    | pos :: tl ->
      if Set.mem walls pos then None
      else if List.mem acc pos ~equal:Coord.equal then inner acc tl
      else
      match Hashtbl.find table pos with
      | None -> inner acc tl
      | Some(bkt) ->
        let np = next pos d in
        match d with
        | Left | Right -> inner (pos :: acc) (np :: tl)
        | _ -> inner (pos :: acc) (np :: (pair pos bkt) :: tl)
    in
    inner [] [first]
  in
  let move_boxes d l =
    let nl = List.map l ~f:(fun p -> (p, Hashtbl.find_exn table p)) in
    List.iter l ~f:(Hashtbl.remove table);
    List.iter nl ~f:(fun (p, bkt) ->
      let np = next p d in
      match d with
      | Up | Down -> Hashtbl.set table ~key:np ~data:bkt
      | _ -> Hashtbl.set table ~key:np ~data:bkt
    )
  in
  let rec inner pos = function
  | [] -> Hashtbl.filter table ~f:(function | Closed -> false | _ -> true) |> Hashtbl.keys
  | d :: ds ->
    let np = next pos d in
    if Hashtbl.mem table np then (* try to push boxes *)
      match boxes_to_move np d with
      | None -> inner pos ds (* push in a wall *)
      | Some(l) ->
        move_boxes d l; (* move boxes if any *)
        inner np ds (* move robot *)
    else if Set.mem walls np then inner pos ds (* hit a wall *)
    else inner np ds (* move robot *)
  in
  inner (24, 48) directions

let part2 =
  let (walls, boxes) = parse_map2 and dirs = parse_directions in
  let l =  _move2 walls boxes dirs in
  score l

let () =
  printf "part1=%d\n" part1;
  printf "part2=%d\n" part2;
