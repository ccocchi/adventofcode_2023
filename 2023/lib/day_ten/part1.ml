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
  mutable in_loop: bool;
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
  { t = t; in_loop = false }

let build_grid str =
  let row_len = String.index str '\n' in
  let ary = Array.make (row_len * row_len) { t = Ground; in_loop = false } in
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
| (Start, _) -> Some(Up)
| _ -> None

let find_start grid = Array.find_index (fun tile -> tile.t == Start) grid.tiles

type connection = int * direction

let find_first_connection (grid: grid) (pos: int): connection =
  let possibilities = [(pos - grid.width, Up); (pos + grid.width, Down); (pos - 1, Left); (pos + 1, Right)] in
  let can_access (i, dir) =
    let tile = Array.get grid.tiles i in
    match next_dir tile dir with
    | Some(_) -> true
    | None -> false
  in
  List.filter can_access possibilities |> List.hd

let print_connection (pos, dir) =
  let print_dir = match dir with
  | Up -> "Up"
  | Down -> "Down"
  | Left -> "Left"
  | Right -> "Right"
  in
  print_char '(';
  print_int pos;
  print_string ", ";
  print_string (print_dir);
  print_endline ")"

let next_connection (grid: grid) (current: connection): connection =
  let next_position = match current with
  | (pos, Up) ->
    let n = pos - grid.width in
    if (pos < grid.width) then None else Some(n)
  | (pos, Down) -> Some(pos + grid.width)
  | (pos, Left) ->
    let n = pos - 1 in
    if (pos mod grid.width == 0) then None else Some(n)
  | (pos, Right) ->
    let n = pos + 1 in
    if (pos mod grid.width == (grid.width - 1)) then None else Some(n)
  in
  let tile = match next_position with
  | None -> failwith "next_connection: impossible"
  | Some(p) -> Array.get grid.tiles p
  in
  tile.in_loop <- true;
  (Option.get next_position, (Option.get (next_dir tile (snd current))))

let find_longest grid (start_position: int) =
  let start = find_first_connection grid start_position in
  let rec inner acc conn = match conn with
  | (pos, _) when pos == start_position -> acc + 1
  | _ -> inner (acc + 1) (next_connection grid conn)
in
let tile = Array.get grid.tiles (fst start) in
tile.in_loop <- true;
inner 0 start

(* The Crossing Number (cn) method *)
let count_enclosing grid =
  let limit = grid.width * grid.height in
  let rec inner cnt is_inside i =
    let tile = Array.get grid.tiles i in
    let is_north_facing = match tile.t with
    | NS -> true
    | NE -> true
    | NW -> true
    | _ -> false
    in
    let next_is_inside = if (is_north_facing && tile.in_loop) then (not is_inside) else is_inside
    in
    if (i == limit - 1) then cnt
    else
      inner (if is_inside && not tile.in_loop then cnt + 1 else cnt) next_is_inside (i + 1)
  in
  inner 0 false 0

let print_grid grid =
  let print_tile i tile =
    let c = match tile.t with
    | NS -> '|'
    | EW -> '-'
    | NE -> 'L'
    | NW -> 'J'
    | SW -> '7'
    | SE -> 'F'
    | Start -> 'S'
    | Ground -> '.'
  in
    if (i > 0 && i mod grid.width == 0) then print_newline();
    if (tile.in_loop) then print_char 'G' else print_char c;
  in
  Array.iteri print_tile grid.tiles;
  print_newline()


let res () =
  let content = read_whole_file "/Users/chris/code/adventofcode_2023/data/day_ten.txt" in
  let grid = build_grid content in
  let start = find_start grid in
  let _ = find_longest grid (Option.get start) in
  count_enclosing grid

