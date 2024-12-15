open Core

let parse_input =
  let r = Re.compile (Re.Posix.re "-?[0-9]{1,}") in
  In_channel.read_lines "/Users/chris/code/adventofcode_2023/2024/data/day14.txt"
  |> List.map ~f:(fun str -> Re.matches ~pos:0 r str |> function
    | j :: i :: vj :: vi :: [] ->
      ((int_of_string i, int_of_string j), (int_of_string vi, int_of_string vj))
    | _ -> raise(Invalid_argument "wrong format")
  )

let move ((i, j), (vi, vj)) (maxi, maxj) n =
  let ni = (i + n * vi) mod maxi
  and nj = (j + n * vj) mod maxj
  in
  (
    (if ni < 0 then maxi - (Int.abs ni) else ni),
    (if nj < 0 then maxj - (Int.abs nj) else nj)
  )

let part1 input n =
  let a, b, c, d =
  List.map input ~f:(fun e -> move e (103, 101) n)
  |> List.fold ~init:((0, 0, 0, 0)) ~f:(fun (q1, q2, q3, q4) (i, j) ->
    if (i < 51 && j < 50) then (q1 + 1, q2, q3, q4)
    else if (i < 51 && j > 50) then (q1, q2 + 1, q3, q4)
    else if (i > 51 && j < 50) then (q1, q2, q3 + 1, q4)
    else if (i > 51 && j > 50) then (q1, q2, q3, q4 + 1)
    else
      (q1, q2, q3, q4)
  )
  in
  a * b * c * d

let part2 input =
  let rec inner n =
    let l = List.map input ~f:(fun e -> move e (103, 101) n) in
    if (List.length (List.dedup_and_sort l ~compare:(Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare)) = (List.length l))
      then n
  else
    inner (n + 1)
in
inner 0

let () =
  let input = parse_input
in
  printf "part1=%d\n" (part1 input 100);
  printf "part2=%d\n" (part2 input)
