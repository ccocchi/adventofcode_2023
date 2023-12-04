let findDigits list =
  let rec inner l = match l with
  | ('0'..'9' as a) :: tail -> a :: inner(tail)
  | 'o' :: 'n' :: 'e' :: tail -> '1' :: inner('e' :: tail)
  | 't' :: 'w' :: 'o' :: tail -> '2' :: inner('o' :: tail)
  | 't' :: 'h' :: 'r' :: 'e' :: 'e' :: tail -> '3' :: inner('e' :: tail)
  | 'f' :: 'o' :: 'u' :: 'r' :: tail -> '4' :: inner(tail)
  | 'f' :: 'i' :: 'v' :: 'e' :: tail -> '5' :: inner('e' :: tail)
  | 's' :: 'i' :: 'x' :: tail -> '6' :: inner(tail)
  | 's' :: 'e' :: 'v' :: 'e' :: 'n' :: tail -> '7' :: inner('n' :: tail)
  | 'e' :: 'i' :: 'g' :: 'h' :: 't' :: tail -> '8' :: inner('t' :: tail)
  | 'n' :: 'i' :: 'n' :: 'e' :: tail -> '9' :: inner('e' :: tail)
  | _ :: tail -> inner(tail)
  | [] -> []
  in
  inner list

let lineToInt line =
  let toInt list = String.make 1 (Batteries.List.first list) ^ String.make 1 (Batteries.List.last list) |> int_of_string in
  line |> Batteries.String.to_list |> findDigits |> toInt

let res =
  let file = "/Users/chris/code/adventofcode_2023/data/day_one.txt" in
  let lines = Batteries.File.lines_of(file) |> Batteries.List.of_enum in
  List.map lineToInt lines |> Batteries.List.sum
