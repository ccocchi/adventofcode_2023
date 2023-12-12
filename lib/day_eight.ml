type rl = Left | Right

let instructions =
  let base = "LRLLLRRLRRLRRLRRLLRRLRRLLRRRLLRRLRRLRRLRRLRLRLLLLLRRLRRLLRLRRRLLRRLRLLLLLLLRRLRLRRRLRRLRRRLRRLLLRRLLRRRLLRRRLRRLRLRRRLRRRLRLRLLRRRLRRRLRRLLRRRLRLRRLLRLLRRLLRRLRRRLRRLRLRRLLRRRLRRRLRRRLRLRLRLRRRLLRRRLRLRRLLRRLRRLRRLRLLRRLLRRRLRRRLRRLRRLRLLRRLRLRRLRRRLRRRLRRLRLRRRLRRRLRLLLRRLRLLRRRR" in
  let length = String.length base in
  Stream.from (fun n -> Some(String.get base (n mod length)))

let nextInstruction () = match Stream.next instructions with
| 'R' -> Right
| _ -> Left

let map = Hashtbl.create 720;;

let rec buildMap acc e =
  let r = Str.regexp "^\\([1-9A-Z]+\\) = (\\([1-9A-Z]+\\), \\([1-9A-Z]+\\))$" in
  match Batteries.Enum.get e with
| Some(l) ->
  (match Str.string_match r l 0 with
  | true ->
    let key = (Str.matched_group 1 l) in
    Hashtbl.add map key (Str.matched_group 2 l, Str.matched_group 3 l);
    buildMap (if (String.ends_with ~suffix: "A" key) then (key :: acc) else acc) e
  | _ -> failwith "buildMap: invalid line");
| None -> acc

let rec part1 acc el = match el with
| "ZZZ" -> acc
| _ -> match (Hashtbl.find_opt map el, nextInstruction()) with
  | (None, _) -> failwith ("part1: missing " ^ el)
  | (Some((a, _)), Left) -> part1 (acc + 1) a
  | (Some((_, b)), Right) -> part1 (acc + 1) b

let part2 el =
  let is =
    let base = "LRLLLRRLRRLRRLRRLLRRLRRLLRRRLLRRLRRLRRLRRLRLRLLLLLRRLRRLLRLRRRLLRRLRLLLLLLLRRLRLRRRLRRLRRRLRRLLLRRLLRRRLLRRRLRRLRLRRRLRRRLRLRLLRRRLRRRLRRLLRRRLRLRRLLRLLRRLLRRLRRRLRRLRLRRLLRRRLRRRLRRRLRLRLRLRRRLLRRRLRLRRLLRRLRRLRRLRLLRRLLRRRLRRRLRRLRRLRLLRRLRLRRLRRRLRRRLRRLRLRRRLRRRLRLLLRRLRLLRRRR" in
    let length = String.length base in
    Stream.from (fun n -> Some(String.get base (n mod length)))
  in
  let next () = match Stream.next is with
    | 'R' -> Right
    | _ -> Left
  in
  let rec inner acc e =
    if (String.ends_with ~suffix: "Z" e)
    then acc
    else match (Hashtbl.find_opt map e, next()) with
    | (None, _) -> failwith ("part2b: missing " ^ e)
    | (Some((a, _)), Left) -> inner (acc + 1) a
    | (Some((_, b)), Right) -> inner (acc + 1) b
  in
  inner 0 el

let rec gcd u v =
  if v <> 0 then (gcd v (u mod v))
  else (abs u)

let lcm m n =
  match m, n with
  | 0, _ | _, 0 -> 0
  | m, n -> abs (m * n) / (gcd m n)

let res () =
  let file = "/Users/chris/code/adventofcode_2023/data/day_eight.txt" in
  let lines = Batteries.File.lines_of(file) in
  let startingPositions = buildMap [] lines in
  List.fold_left lcm 1 (List.map part2 startingPositions)


