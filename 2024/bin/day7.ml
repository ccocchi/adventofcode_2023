open Core

let has_loop (list: int list) =
  let rec inner l1 l2 = match (l1, l2) with
  | (x :: xs, _ :: y :: ys) -> Int.equal x y || inner xs ys
  | _ -> false
in
  inner list list

let () =
  printf "%b\n" (has_loop [2;0;6;3;1;4;2;5;3;1;4;2])
