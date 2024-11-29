let parseLine l =
  let sep = Str.regexp ":?[ \t]+" in
  match Str.split sep l with
  | [] -> failwith "parseLine: empty line"
  | _ :: tail -> List.map int_of_string tail

let parse e fn = match Batteries.Enum.get e with
| None -> failwith "parse: fail"
| Some(l1) -> match Batteries.Enum.get e with
  | None -> failwith "parse: fail"
  | Some(l2) -> List.map2 fn (parseLine l1) (parseLine l2)

let part1 time distance =
  let run charge = (time - charge) * (charge) in
  let rec inner acc t = match ((run t), t) with
  | (_, -1) -> acc
  | (n, r) -> inner (acc + (if n >= distance then 1 else 0)) (r - 1)
  in
  inner 0 time

let res () =
  let file = "/Users/chris/code/adventofcode_2023/data/day_six.txt" in
  let lines = Batteries.File.lines_of(file) in
  List.fold_left (fun acc x -> acc * x) 1 (parse lines part1)


