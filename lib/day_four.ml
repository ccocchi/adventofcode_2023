let score winning numbers =
  let s1 = Batteries.Set.of_list(winning) in
  let s2 = Batteries.Set.of_list(numbers) in
  match Batteries.Set.cardinal (Batteries.Set.intersect s1 s2) with
  | 0 -> 0
  | 1 -> 1
  | n -> Batteries.Int.pow 2 (n - 1)

let count winning numbers =
  let s1 = Batteries.Set.of_list(winning) in
  let s2 = Batteries.Set.of_list(numbers) in
  Batteries.Set.cardinal (Batteries.Set.intersect s1 s2)

let makeCard line fn =
  let i = int_of_string (String.trim (String.sub line 5 3)) in
  let sep = Str.regexp "[ \t]+" in
  let winning = Str.split sep (Batteries.String.slice line ~first: 10 ~last: 39) in
  let numbers = Str.split sep (Batteries.String.slice line ~first: 42) in
  (i, (fn winning numbers))

let part1 l = List.map (fun x -> makeCard x score |> snd) l |> Batteries.List.sum

let process list =
  let stack =
    let s = Stack.create() in
    List.iter (fun x -> Stack.push x s) (List.rev list);
    s
  and reserve = Array.of_list(list)
  in
  let rec loop acc = match (Stack.pop_opt stack) with
  | Some((i, 0)) -> (loop [@tailcall]) (i :: acc)
  | Some((i, wins)) ->
    for x = i to (i + wins - 1) do
      Stack.push (Array.get reserve x) stack
    done;
    (loop [@tailcall]) (i :: acc)
  | None -> acc
  in
  loop []

let part2 l = (List.map (fun x -> makeCard x count) l) |> process |> List.length

let res () =
  let file = "/Users/chris/code/adventofcode_2023/data/day_four.txt" in
  let lines = Batteries.File.lines_of(file) |> Batteries.List.of_enum in
  part2 lines
