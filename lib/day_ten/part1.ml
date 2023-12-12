let read_whole_file filename =
  (* open_in_bin works correctly on Unix and Windows *)
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

type tile_type = NS | EW | NE | NW | SW | SE | Start | Ground
type direction = Up | Down | Left | Right

type tile = {
  t: tile_type;
  mutable marked: bool;
}

type grid = {
  tiles: tile array;
  width: int;
  height: int;
}

let char_to_tile c =
  let t = match c with
  | '|' -> NS
  | '-' -> EW
  | 'L' -> NE
  | 'J' -> NW
  | '7' -> SW
  | 'F' -> SE
  | 'S' -> Start
  | _ -> Ground
in
  { t = t; marked = false }

let build_grid str =
  let row_len = String.index str '\n' in
  let ary = Array.make (row_len * row_len) { t = Ground; marked = false } in
  let no_newline_str = Str.global_replace (Str.regexp "\n") "" str in
  String.iteri (fun i c -> if (c <> '.') then Array.set ary i (char_to_tile c)) no_newline_str;
  {
    tiles = ary;
    width = row_len;
    height = row_len;
  }

let next_dir tile from = match (tile.t, from) with
| (NS, Up) -> Some(Up)
| (NS, Down) -> Some(Down)
| (EW, Left) -> Some(Left)
| (EW, Right) -> Some(Right)
| (NE, Down) -> Some(Right)
| (NE, Left) -> Some(Up)
| (NW, Down) -> Some(Left)
| (NW, Right) -> Some(Up)
| (SW, Up) -> Some(Left)
| (SW, Right) -> Some(Down)
| (SE, Up) -> Some(Right)
| (SE, Left) -> Some(Down)
| _ -> None

let next_tile grid tp = match tp with
| (pos, Some(Up)) ->
  let n = pos - grid.width in
  if (pos < grid.width) then (-1, None)
  else (n, next_dir (Array.get grid.tiles n) Up)
| (pos, Some(Down)) ->
  let n = pos + grid.width in
  (n, next_dir (Array.get grid.tiles n) Down)
| (pos, Some(Left)) ->
  let n = pos - 1 in
  if (pos mod grid.width == 0) then (-1, None)
  else(n, next_dir (Array.get grid.tiles n) Left)
| (pos, Some(Right)) ->
  let n = pos + 1 in
  if (pos mod grid.width == (grid.width - 1)) then (-1, None)
  else (n, next_dir (Array.get grid.tiles n) Right)
| _ -> (-1, None)

let listToTuple = function
| [] -> failwith("listToTuple: error")
| a :: tail -> match tail with
  | b :: [] -> (a, b)
  | _ -> failwith("listToTuple: error")

let find_start grid = Array.find_index (fun tile -> tile.t == Start) grid.tiles

let find_connected_neighbors grid pos =
  let tiles = [(pos, Some(Up)); (pos, Some(Down)); (pos, Some(Left)); (pos, Some(Right))]
  in
  List.map (next_tile grid) tiles |> List.filter (fun (_, x) -> match x with
  | Some(_) -> true
  | None -> false)

let find_longest grid start =
  let neighbors = find_connected_neighbors grid start |> listToTuple in
  let rec inner acc positions = match positions with
  | ((p1, _), (p2, _)) when p1 == p2 -> acc + 1
  | (tp1, tp2) -> inner (acc + 1) (next_tile grid tp1, next_tile grid tp2)
in
inner 0 neighbors

let res () =
  let content = read_whole_file "/Users/chris/code/adventofcode_2023/data/day_ten.txt" in
  let grid = build_grid content in
  let start = find_start grid in
  find_longest grid (Option.get start)

