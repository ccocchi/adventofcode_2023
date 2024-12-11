open Core

let parse_input =
  In_channel.read_all "/Users/chris/code/adventofcode_2023/2024/data/day11.txt"
  |> String.strip
  |> String.split ~on:' '
  |> List.map ~f:(fun str -> (str, int_of_string str))

let loop size ~init ~f =
  let rec inner acc = function
  | 0 -> acc
  | n -> inner (f (List.rev acc)) (n - 1)
in
  inner init size

let blink l = List.fold l ~init:[] ~f:(fun acc -> function
  | (_, 0) -> ("1", 1) :: acc
  | (str, v) ->
    let len = String.length str in
    if (len mod 2 = 0) then
      let h = len / 2 in
      let v1 = String.prefix str h |> int_of_string
      and v2 = String.suffix str h |> int_of_string
      in
      (string_of_int v2, v2) :: (string_of_int v1, v1) :: acc
    else
      let nv = v * 2024 in (string_of_int nv, nv) :: acc
) |> List.rev

let blink_one n stone =
  let inner acc = function
  | (_, 0) -> ("1", 1) :: acc
  | (str, v) ->
    let len = String.length str in
    if (len mod 2 = 0) then
      let h = len / 2 in
      let v1 = String.prefix str h |> int_of_string
      and v2 = String.suffix str h |> int_of_string
      in
      (string_of_int v2, v2) :: (string_of_int v1, v1) :: []
    else
      let nv = v * 2024 in [(string_of_int nv, nv)]
  in
    loop n ~init:[] ~f:


let part1 list = loop 26 ~init:list ~f:blink
  |> List.length

let () =
  let input = parse_input in
  printf "part1=%d\n" (part1 input)
