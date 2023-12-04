type drawResult = {
  red: int;
  blue: int;
  green: int;
}

type colour = Red | Blue | Green

let initialResult = { red = 1; blue = 1; green = 1 }

let countColor str =
  let inner list = match list with
  | v :: "green" :: [] -> (int_of_string v, Green)
  | v :: "red" :: [] -> (int_of_string v, Red)
  | v :: "blue" :: [] -> (int_of_string v, Blue)
  | _ -> raise(Invalid_argument (List.fold_left (fun acc s -> acc ^ " " ^ s) "" list))
  in
    String.split_on_char ' ' (String.trim str) |> inner

let drawToResult d =
  let inner acc c = match c with
  | (v, Green) -> { acc with green = v }
  | (v, Red) -> { acc with red = v }
  | (v, Blue) -> { acc with blue = v }
  in
  String.split_on_char ',' d |> List.map countColor |> List.fold_left inner initialResult

let gameToResult str =
  let draws = (String.split_on_char ';' str) in
  let combineResult a b = { red = (max a.red b.red); blue = (max a.blue b.blue); green = (max a.green b.green) }
  in
  List.map drawToResult draws |> List.fold_left combineResult initialResult

let lineToTuple line =
  let list = String.split_on_char ':' line in
  (int_of_string (Str.string_after (Batteries.List.first list) 5), gameToResult (Batteries.List.last list))

let isPossible = function
| (id, { red; blue; green }) -> if (red <= 12 && green <= 13 && blue <= 14) then id else 0

let power = function
| (_, { red; blue; green }) -> red * blue * green

let res =
  let file = "/Users/chris/code/adventofcode_2023/data/day_two.txt" in
  let lines = Batteries.File.lines_of(file) |> Batteries.List.of_enum in
  let fn s = s |> lineToTuple |> power in
  List.map fn lines |> Batteries.List.sum
