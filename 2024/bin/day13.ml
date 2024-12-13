open Core

let parse_input =
  let rb = Re.compile (Re.Posix.re "\\+([0-9]{1,})")
  and rp = Re.compile (Re.Posix.re "=([0-9]{1,})")
  and extract_values str r = Re.all ~pos:0 r str |> function
  | g1 :: g2 :: [] ->(int_of_string (Re.Group.get g1 1), int_of_string (Re.Group.get g2 1))
  | _ -> raise(Invalid_argument "bad button")
  in
  let rec inner acc = function
  | a :: b :: prize :: _ :: tl ->
    let m = (extract_values a rb, extract_values b rb, extract_values prize rp) in
    inner (m :: acc) tl
  | _ -> acc
  in
  inner [] (In_channel.read_lines "/Users/chris/code/adventofcode_2023/2024/data/day13.txt")

(* Cramer's rule *)
let solve ((a1, a2), (b1, b2), (c1, c2)) =
  let detA = Int.abs (a1 * b2 - b1 * a2) in
  if (detA = 0) then None
  else
    let det1 = Int.abs (c1 * b2 - b1 * c2)
    and det2 = Int.abs (a1 * c2 - c1 * a2)
    in
    if (det1 mod detA = 0 && det2 mod detA = 0) then
      Some(det1 / detA, det2 / detA)
    else
      None

let part1 l =
  List.fold l ~init:0 ~f:(fun acc x -> match solve x with
  | Some(x, y) when x <= 100 && y <= 100 -> acc + x * 3 + y
  | _ -> acc
  )

let part2 l n =
  List.fold l ~init:0 ~f:(fun acc (a, b, c) ->
    let nx = (a, b, ((fst c) + n, (snd c) + n)) in
    match solve nx with
  | Some(x, y) -> acc + x * 3 + y
  | _ -> acc
  )

let () =
  let input = parse_input in
  printf "part1=%d\n" (part1 input);
  printf "part2=%d\n" (part2 input 10000000000000)


