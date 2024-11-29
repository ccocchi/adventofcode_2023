open Core

let read_lines =
  In_channel.read_lines "/Users/chris/code/adventofcode_2023/data/day21.txt"
  |> List.map ~f:String.to_array
  |> Array.of_list

module Position = struct
  module T = struct
    type t = int * int [@@deriving ord, sexp]
  end

  include T
  include Comparator.Make(T)
end

let part1 mat (start: Position.t) =
  let m, n = (Array.length mat, Array.length mat.(0)) in
  let rec advance positions = function
  | 0 -> positions
  | step ->
    let new_pos = Set.to_list positions
      |> List.concat_map ~f:(fun (i, j) ->
        [(i + 1, j); (i - 1, j); (i, j - 1); (i, j + 1)]
        |> List.filter ~f:(fun (x, y) -> x >= 0 && x < m && y >= 0 && y < n && Char.equal '.' mat.(x).(y))
      )
      |> Set.of_list (module Position)
    in
    advance new_pos (step - 1)
  in
  mat.(fst start).(snd start) <- '.';
  let set = Set.of_list (module Position) [start] in
  advance set 64 |> Set.length

let () =
  let mat = read_lines in
  printf "part1=%d\n" (part1 mat (65, 65))



