open Core

let parse_input =
  In_channel.read_all "/Users/chris/code/adventofcode_2023/2024/data/day11.txt"
  |> String.strip
  |> String.split ~on:' '
  |> List.map ~f:(fun str -> (str, int_of_string str))

type stone = string * int

module CacheKey = struct
  type t = int * int [@@deriving compare, equal, sexp_of, hash]
end

let next = function
| (_, 0) -> [("1", 1)]
| (str, v) ->
  let len = String.length str in
  if (len mod 2 = 0) then
    let h = len / 2 in
    let v1 = String.prefix str h |> int_of_string
    and v2 = String.suffix str h |> int_of_string
    in
    (string_of_int v1, v1) :: (string_of_int v2, v2) ::  []
  else
    let nv = v * 2024 in [(string_of_int nv, nv)]

let blink loop_size list =
  let cache = Hashtbl.create (module CacheKey) in
  let rec inner size l =
    let blink_one (n: int) (s: stone): int =
      let key = (n, snd s) in
      match Hashtbl.find cache key with
      | Some(v) -> v
      | None ->
        let v = inner (n - 1) (next s) in
        Hashtbl.add_exn cache ~key ~data:v;
        v
    in
    match size with
    | 0 -> List.length l
    | n -> List.map l ~f:(fun stone -> blink_one n stone) |> List.reduce_exn ~f:(+)
  in
  inner loop_size list

let part1 list = blink 25 list

let part2 list = blink 75 list

let () =
  let input = parse_input in
  printf "part1=%d\n" (part1 input);
  printf "part2=%d\n" (part2 input)
