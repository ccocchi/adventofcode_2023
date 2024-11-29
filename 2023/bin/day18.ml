open Core

module Direction = struct
  type t = Up | Down | Left | Right

  let of_string s = match s.[0] with
  | 'U' -> Up
  | 'D' -> Down
  | 'L' -> Left
  | 'R' -> Right
  | _ -> failwith "wrong direction"
end

let read_lines =
  In_channel.read_lines "/Users/chris/code/adventofcode_2023/data/day18.txt"
  |> List.map ~f:(fun l -> l |> String.split ~on:' ' |> function
  | [d; w; color] -> (Direction.of_string d, Int.of_string w, color)
  | _ -> failwith "bad format")

let shoelace instructions =
  let x = ref 0 and y = ref 0 in
  let cnt = List.fold instructions ~init:0 ~f:(fun acc (dir, n, _) ->
    let x', y' = match dir with
      | Direction.Up -> (!x - n, !y)
      | Direction.Down -> (!x + n, !y)
      | Direction.Left -> (!x, !y - n)
      | Direction.Right -> (!x, !y + n)
    in
    let res = acc + n + ((y' + !y) * (x' - !x)) in
    x := x';
    y := y';
    res
  )
  in cnt / 2 + 1

let hexa_string_to_int s =
  let int_of_char ch = match ch with
    | '0' -> 0
    | '1' -> 1
    | '2' -> 2
    | '3' -> 3
    | '4' -> 4
    | '5' -> 5
    | '6' -> 6
    | '7' -> 7
    | '8' -> 8
    | '9' -> 9
    | 'a' -> 10
    | 'b' -> 11
    | 'c' -> 12
    | 'd' -> 13
    | 'e' -> 14
    | 'f' -> 15
    | c -> failwith ("wrong hex value " ^ String.of_char c)
  in
  String.to_list s |> List.rev |> List.foldi ~init:0 ~f:(fun i acc c ->
    acc + (int_of_char c) * Int.of_float (16. ** Float.of_int i)
)

let convert instructions =
  let char_to_direction = function
    | '0' -> Direction.Right
    | '1' -> Direction.Down
    | '2' -> Direction.Left
    | '3' -> Direction.Up
    | _ -> failwith "wrong direction"
  in
  List.map instructions ~f:(fun (_, _, str) ->
    let hex = String.sub str ~pos:2 ~len: 5 in
    (char_to_direction str.[7], hexa_string_to_int hex, str)
  )

let () =
  let instructions = read_lines in
  printf "part1=%d\n" (shoelace instructions);
  printf "part2=%d\n" (shoelace (convert instructions))
