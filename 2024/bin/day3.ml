open Core

let parse_input = In_channel.read_all "/Users/chris/code/adventofcode_2023/2024/data/day3.txt"

let r = Re.compile (Re.Posix.re "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)")

let multiply_list l =
  List.map ~f:(fun g -> (Re.Group.get g 1, Re.Group.get g 2)) l
  |> List.fold ~init:Int.zero ~f:(fun acc (a, b) -> acc + (int_of_string a) * (int_of_string b))

let part1 (input: string) = Re.all ~pos:0 r input |> multiply_list

type mode = On | Off

let find_enabled_ranges (input: string) =
  let do_pattern = String.Search_pattern.create ~case_sensitive:true "do()"
  and dont_pattern = String.Search_pattern.create ~case_sensitive:true "don't()"
in
  let rec inner acc pos mode = match mode with
  | On -> (
    match String.Search_pattern.index ~pos: pos dont_pattern ~in_:input with
    | Some(stop) -> inner ((pos, stop) :: acc) (stop + 7) Off
    | None -> (pos, String.length input) :: acc
  )
  | Off -> (
     match String.Search_pattern.index ~pos: pos do_pattern ~in_:input with
    | Some(stop) -> inner acc (stop + 4) On
    | None -> acc
)
in
  inner [] 0 On

let part2 input =
  find_enabled_ranges input
  |> List.map ~f:(fun (start, stop) -> Re.all ~pos:start ~len:(stop - start) r input)
  |> List.concat
  |> multiply_list

let () =
  let input = parse_input in
  printf "part1=%d\n" (part1 input);
  printf "part2=%d\n" (part2 input)
