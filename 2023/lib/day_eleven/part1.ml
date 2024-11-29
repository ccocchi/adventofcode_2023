let read_whole_file filename =
  (* open_in_bin works correctly on Unix and Windows *)
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let galaxies = Hashtbl.create 500

let map_the_universe width i c =
  let cnt = ref 0
  in match c with
  | '#' ->
    let x = i mod width and y = i / width in
    cnt := !cnt + 1;
    Hashtbl.add galaxies !cnt (x, y)
  | _ -> ()

let find_empty fn (list: (int * int) list) stop =
  let rec inner i acc = if i == stop then acc
  else
    inner (i + 1) (if List.exists (fn i) list then acc else i :: acc)
  in
  List.rev (inner 0 [])

let find_empty_rows = find_empty (fun i (_, y) -> y == i)
let find_empty_cols = find_empty (fun i (x, _) -> x == i)

let combine_galaxies list =
  let rec inner acc l = match l with
    | hd :: tail -> inner ((List.map (fun x -> (hd, x)) tail) :: acc) tail
    | [] -> acc
  in
  List.flatten (inner [] list)

let expand_universe list col_offsets row_offsets =
  let rec offset x res = function
    | e :: _ when x < e -> res
    | _ :: tail -> offset x (res + 999999) tail
    | [] -> res
  in
  let move_galaxy (x, y) =
    (x + (offset x 0 col_offsets), y + (offset y 0 row_offsets))
  in
  List.map move_galaxy list

let res () =
  let str = read_whole_file "/Users/chris/code/adventofcode_2023/data/day_eleven.txt" in
  let width = String.index str '\n' in
  let content = Str.global_replace (Str.regexp "\n") "" str in
  let height = (String.length content) / width in
  let galaxies_list =
    String.iteri (map_the_universe width) content;
    Hashtbl.fold (fun _ v acc -> v :: acc) galaxies []
  in
  let expanded_list =
    expand_universe galaxies_list (find_empty_cols galaxies_list width) (find_empty_rows galaxies_list height)
  in
    List.map (fun ((x1, y1), (x2, y2)) -> Int.abs (x1 - x2) + Int.abs (y1 - y2)) (combine_galaxies expanded_list) |> Batteries.List.sum

