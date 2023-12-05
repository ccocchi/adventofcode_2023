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

let makeMap (lines: string list) =
  let tuples = List.map (fun l -> String.split_on_char ' ' l |> listToTuple) lines in
  let rec mk m t = match t with
  | (_, _, 0) -> m
  | (dst, src, n) -> mk (Batteries.Map.Int.add (src + n - 1) (dst + n - 1) m) (dst, src, n - 1)
  in
  List.fold_left mk Batteries.Map.Int.empty tuples

let soil_of_seed (seed: int) (list: int BatMap.Int.t list): int =
  List.fold_left (fun acc m -> Batteries.Map.Int.find_default acc acc m) seed list

let part1 seeds l = List.map (fun s -> soil_of_seed s l) seeds |> Batteries.List.min

let res () =
  let file = "/Users/chris/code/adventofcode_2023/data/day_five.txt" in
  let e = Batteries.File.lines_of(file) in
  let rec inner seeds maps = match Batteries.Enum.get e with
  | None ->
    part1 seeds (List.rev maps)
  | Some(l) when String.starts_with ~prefix: "seeds:" l ->
      let s = Batteries.String.slice ~first: 7 l in
      inner (String.split_on_char ' ' s |> List.map int_of_string) maps
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


