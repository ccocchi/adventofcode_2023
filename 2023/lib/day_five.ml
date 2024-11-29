let m = Batteries.Map.Int.empty

let bar () = Batteries.Map.Int.add 10 10 m

let listToTuple l = match l with
| [] -> failwith "listToTuple: not enough elements"
| a :: a_tail -> match a_tail with
  | [] -> failwith "listToTuple: not enough elements"
  | b :: b_tail -> match b_tail with
    | c :: [] -> (int_of_string a, int_of_string b, int_of_string c)
    | [] -> failwith "listToTuple: not enough elements"
    | _ -> failwith "listToTuple: too many elements"

let makeMap (lines: string list) = List.map (fun l -> String.split_on_char ' ' l |> listToTuple) lines

let soil_of_seed (seed: int) (list: (int * int * int) list list): int =
  let rec next s l = match l with
  | (dst, src, n) :: _ when s >= src && s < (src + n) -> dst + (s - src)
  | _ :: tail -> next s tail
  | [] -> s
  in
  List.fold_left next seed list

let part1 seeds l = List.map (fun s -> soil_of_seed s l) seeds |> Batteries.List.min

let part2 seeds maps =
  let inner rng = (List.map (fun x -> soil_of_seed x maps) (Lazy.force rng)) |> Batteries.List.min in
  List.map inner seeds |> Batteries.List.min

let buildSeeds l =
  let rec inner res l = match l with
  | start :: length :: tail -> inner (lazy (Batteries.List.range start `To (start + length - 1)) :: res) tail
  | [] -> res
  | _ -> failwith "buildSeeds: invalid list of seeds"
  in
  String.split_on_char ' ' l |> List.map int_of_string |> inner []

let res () =
  let file = "/Users/chris/code/adventofcode_2023/data/day_five.txt" in
  let e = Batteries.File.lines_of(file) in
  let rec inner seeds maps = match Batteries.Enum.get e with
  | None ->
    (* part1 seeds (List.rev maps) *)
    part2 seeds (List.rev maps)
  | Some(l) when String.starts_with ~prefix: "seeds:" l ->
      let s = Batteries.String.slice ~first: 7 l in
      (* inner (String.split_on_char ' ' s |> List.map int_of_string) maps *)
      inner (buildSeeds s) maps
  | Some(l) when String.ends_with ~suffix: "map:" l ->
      let rec buildLines lines = match Batteries.Enum.get e with
      | Some(k) when Batteries.String.is_empty k -> makeMap lines
      | Some(k) -> buildLines (k :: lines)
      | None -> makeMap lines
      in
      inner seeds ((buildLines []) :: maps)
  | _ -> inner seeds maps
  in
  inner [] []


