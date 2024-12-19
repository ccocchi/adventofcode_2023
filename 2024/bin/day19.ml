open Core

let parse_towels =
  In_channel.read_all "/Users/chris/code/adventofcode_2023/2024/data/day19_1.txt"
  |> String.split ~on:','
  |> List.map ~f:String.strip
  |> List.map ~f:(fun s -> (s, String.length s))
  |> List.sort ~compare:(fun (_, l1) (_, l2) -> Int.compare l2 l1)

let parse_patterns =
  In_channel.read_lines "/Users/chris/code/adventofcode_2023/2024/data/day19_2.txt"

(* module Tree = struct
  type tree =
  | Empty
  | Root of tree list
  | Node of string * tree list

  let add t e =
    let rec add_l list = match list with
    | [] -> [Node(e, [])]
    | Node(v, l) :: tl when String.is_prefix e ~prefix:v -> Node(v, add_l l) :: tl
    | Node(v, _) :: _ when String.compare e v < 0 -> Node(e, []) :: list
    | n :: tl -> n :: (add_l tl)
    in
    match t with
    | Empty -> Root([Node(e, [])])
    | Root(l) -> Root(add_l l)
    | _ -> invalid_arg("")

  let rec iter ~f = function
  | Empty -> ()
  | Root(l) -> List.iter l ~f:(fun t -> iter ~f t)
  | Node(v, l) -> f(v); List.iter l ~f:(fun t -> iter ~f t)
end

let build_tree towels =
  List.fold towels ~init:Tree.Empty ~f:Tree.add |> Tree.iter ~f:print_endline *)


let solve cache pattern ~towels =
  let rec inner str = match Hashtbl.find cache str with
  | None ->
    let res = List.fold ~init:0 towels ~f:(fun acc (t, l) ->
      let len = String.length str in
      if len = 0 then 1
      else if len < l then acc
      else if String.is_prefix str ~prefix:t then acc + (inner (String.drop_prefix str l))
      else
        acc
    )
    in
    Hashtbl.add_exn cache ~key:str ~data:res;
    res
  | Some(v) -> v
  in
  inner pattern

let () =
  let towels = parse_towels and patterns = parse_patterns in
  let cache = Hashtbl.create (module String) in
  let l = List.map patterns ~f:(fun p -> solve cache p ~towels) in
  printf "part1=%d\n" (List.count l ~f:(fun x -> x > 0));
  printf "part2=%d\n" (List.fold l ~init:0 ~f:(+))
