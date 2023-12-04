let symbolLocations = Hashtbl.create 500;;

let numberLocations = Hashtbl.create 500;;

let getPosition i width =
  let y = i / width in
  let x = i mod width in
  (x, y)

let buildNumbersList chars width =
  let rec inner list acc i = match list with
  | '0'..'9' as c :: tail ->
    let sp = Batteries.List.span Batteries.Char.is_digit tail in
    let str = Batteries.String.of_list (c :: (fst sp)) in
    let e = (getPosition i width, int_of_string str, String.length str) in
    inner (snd sp) (e :: acc) (i + String.length str)
  | '.' :: tail -> inner tail acc (i + 1)
  | c :: tail ->
      Hashtbl.add symbolLocations (getPosition i width) c;
      inner tail acc (i + 1)
  | [] -> acc
  in
  inner chars [] 0

let buildGearsList chars width =
  let rec inner list acc i = match list with
  | '0'..'9' as c :: tail ->
    let sp = Batteries.List.span Batteries.Char.is_digit tail in
    let str = Batteries.String.of_list (c :: (fst sp)) in
    let n = int_of_string str in
    for x = i to (i + width) do
      Hashtbl.add numberLocations (getPosition n width) (x, n);
    done;
    inner tail acc (i + String.length str)
  | '*' :: tail -> print_int i; print_newline(); inner tail ((getPosition i width) :: acc) (i + 1)
  | _ :: tail -> inner tail acc (i + 1)
  | [] -> acc
  in
  inner chars [] 0

let hasSymbolNearby =
  let checkLine y start offset =
    let makePos = (fun x -> (x, y)) in
    let list = Batteries.List.range start `To (start + offset) in
    List.map makePos list |> List.exists (Hashtbl.mem symbolLocations)
  in
  function
  | ((x, y), _, width) ->
    Hashtbl.mem symbolLocations (x - 1, y) ||
    Hashtbl.mem symbolLocations (x + width, y) ||
    checkLine (y - 1) (x - 1) (width + 1) ||
    checkLine (y + 1) (x - 1) (width + 1)

let aroundNumbers =
  let getLine y start =
    let makePos = (fun x -> (x, y)) in
    let list = Batteries.List.range start `To (start + 2) in
    List.map makePos list |> List.map (Hashtbl.find_opt numberLocations)
  in
  function
| (x, y) ->
  (Hashtbl.find_opt numberLocations (x - 1, y) ::
  Hashtbl.find_opt numberLocations (x + 1, y) ::
  List.append (getLine (y - 1) (x - 1)) (getLine (y + 1) (x - 1))) |> List.filter_map Fun.id

let multiplyGears (list : (int * int) list) =
  let uniqueNumbers = Batteries.List.unique list in
  if ((List.length list) == 2) then
    let ns = List.map snd uniqueNumbers in
    List.fold_left Int.mul 0 ns
  else 0

(* let part1 =
  let file = "/Users/chris/code/adventofcode_2023/data/day_three.txt" in
  let lines = Batteries.File.lines_of(file) |> Batteries.List.of_enum |> List.map Batteries.String.to_list in
  let width = List.length (Batteries.List.first lines) in
  let mid = fun (_, e, _) -> e in
  List.filter hasSymbolNearby (buildNumbersList (List.flatten lines) width) |> List.map mid |> Batteries.List.sum *)

let res =
  let file = "/Users/chris/code/adventofcode_2023/data/day_three.txt" in
  let lines = Batteries.File.lines_of(file) |> Batteries.List.of_enum |> List.map Batteries.String.to_list in
  let width = List.length (Batteries.List.first lines) in
  (* let mid = fun (_, e, _) -> e in *)
  print_string (Batteries.String.of_list (List.flatten lines));
  let l = buildGearsList (List.flatten lines) width in
    List.iter (fun x -> match x with
    | (a, b) -> print_int a; print_char ' '; print_int b; print_newline()) l
  (* |> List.map aroundNumbers |> List.map multiplyGears |> Batteries.List.sum *)

  (* List.filter hasSymbolNearby (buildNumbersList (List.flatten lines) width) |> List.map mid |> Batteries.List.sum *)

