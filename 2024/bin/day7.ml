open Core

let parse_input =
  In_channel.read_lines "/Users/chris/code/adventofcode_2023/2024/data/day7.txt"
  |> List.map ~f:(fun l -> match String.split ~on:':' l with
    | res :: op :: _ -> (res, String.split ~on:' ' (String.lstrip op))
    | _ -> raise(Invalid_argument("a"))
  )
  |> List.map ~f:(fun (x, l) -> (int_of_string x, List.map ~f:int_of_string l))

let isSolvable expected inputs with_concat =
  let rec inner n l =
    if (n < 0) then false
    else
      match l with
      | [] -> true
      | e :: [] -> Int.equal n e
      | x :: xs ->
        (Int.equal Int.zero (n mod x) && inner (n / x) xs)
        || inner (n - x) xs
        || with_concat && (
          let str = string_of_int n and s = string_of_int x in
          try
            String.is_suffix str ~suffix:s && inner (int_of_string (String.drop_suffix str (String.length s))) xs
          with
          | Failure(_) -> true
        )
in
  inner expected (List.rev inputs)

let part1 (equations: (int * int list) list) =
  List.filter ~f:(fun (expected, inputs) -> isSolvable expected inputs false) equations
  |> List.fold ~init:Int.zero ~f:(fun acc (x, _) -> acc + x)

let part2 (equations: (int * int list) list) =
  List.filter ~f:(fun (expected, inputs) -> isSolvable expected inputs true) equations
  |> List.fold ~init:Int.zero ~f:(fun acc (x, _) -> acc + x)

let () =
  let input = parse_input
in
  printf "part1=%d\n" (part1 input);
  printf "part2=%d\n" (part2 input)
