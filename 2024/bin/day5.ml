open Core

let populate_rules table =
  In_channel.read_lines "/Users/chris/code/adventofcode_2023/2024/data/day5_1.txt"
  |> List.iter ~f:(fun line -> match String.split ~on:'|' line with
  | a :: b :: _ -> (
    match Hashtbl.find table b with
    | Some(l) -> Hashtbl.set table ~key:b ~data:(a :: l)
    | None -> Hashtbl.set table ~key:b ~data:[a]
  )
  | _ -> raise(Invalid_argument(""))
  )

let parse_input =
  In_channel.read_lines "/Users/chris/code/adventofcode_2023/2024/data/day5_2.txt"
  |> List.map ~f:(String.split ~on: ',')

let isValidUpdate table list =
  let rec inner l seen = match l with
  | [] -> true
  | e :: tail -> match Hashtbl.find table e with
    | None -> inner tail (e :: seen)
    | Some(needed) ->
      if (List.for_all needed ~f:(fun x -> List.exists seen ~f:(String.equal x) || not (List.exists l ~f:(String.equal x)))) then
        inner tail (e :: seen)
      else
        false
in
  inner list []

let cmp (_, a) (_, b) = Int.compare a b

let reorder table list =
  List.map list ~f:(fun e -> match Hashtbl.find table e with
  | None -> (e, 0)
  | Some(l) -> (e, List.count l ~f:(fun x -> List.exists list ~f:(String.equal x)))
  )
  |> List.sort ~compare:cmp
  |> List.map ~f:fst

let part1 rules input =
  List.filter input ~f:(isValidUpdate rules)
  |> List.map ~f:(fun l -> List.nth_exn l (List.length l / 2))
  |> List.fold ~init:Int.zero ~f:(fun acc x -> acc + int_of_string x)

let part2 rules input =
  List.filter input ~f:(fun l -> isValidUpdate rules l |> not)
  |> List.map ~f:(fun l -> reorder rules l)
  |> List.map ~f:(fun l -> List.nth_exn l (List.length l / 2))
  |> List.fold ~init:Int.zero ~f:(fun acc x -> acc + int_of_string x)

let () =
  let rules = Hashtbl.create (module String)
  and input = parse_input
in
  populate_rules rules;
  printf "part1=%d\n" (part1 rules input);
  printf "part1=%d\n" (part2 rules input)
