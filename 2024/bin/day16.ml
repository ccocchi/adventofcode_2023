open Core
open Aoc_2024

module Coord = struct
  module T = struct
    type t = int * int [@@deriving equal, hash, compare, sexp_of]
  end

  include T
  include Comparator.Make(T)

  let _print (i, j) = printf "%d, %d" i j
end

let parse_input =
  In_channel.read_lines "/Users/chris/code/adventofcode_2023/2024/data/day16.txt"
  |> List.filter_mapi ~f:(fun i s ->
    String.to_list s
    |> List.filter_mapi ~f:(fun j -> function | '#' -> Some((i, j)) | _ -> None)
    |> function | [] -> None | l -> Some(l)
  )
  |> List.concat
  |> Set.of_list (module Coord)

type dir = Up | Down | Left | Right [@@deriving equal]

let opposite = function
| Up -> Down
| Down -> Up
| Left -> Right
| Right -> Left

let neighbors walls (i, j) dir =
  let inner pos d =
    if Set.mem walls pos then None
    else
      let coord = (pos, d)
      and score = if equal_dir d dir then 1 else 1001
      in
      Some((score, coord))
  in
  [
    inner (i - 1, j) Up;
    inner (i + 1, j) Down;
    inner (i, j - 1) Left;
    inner (i, j + 1) Right;
  ]
  |> List.filter_map ~f:(fun x -> x)
  |> List.filter ~f:(fun (_, (_, d)) -> equal_dir d (opposite dir) |> not)

(* let next walls (i, j) dir =
  let np = match dir with
  | Up -> (i - 1, j)
  | Down -> (i + 1, j)
  | Left -> (i, j - 1)
  | Right -> (i, j + 1)
in
  if Set.mem walls np then None else Some(np) *)

type step = {
  pos: Coord.t;
  dir: dir;
  path: (Coord.t * dir) list
}

(* let compare_score (s1, _) (s2, _) = Int.compare s1 s2 *)

let find_path walls start finish =
  let seen = Hashtbl.create (module Coord)
  and openSet = Min_heap.create ~size:64 ~init:{ pos = (0, 0); dir = Up; path = []}
  and paths = ref [] in
  (* let go_back best_path =
    let rec inner res = function
    | [] -> res
    | (pos, dir) :: xs ->
      let ns = neighbors walls pos dir in
      let (best_neighbor_score, (_, best_neighbor_dir)) = ns |> List.sort ~compare:compare_score |> List.hd_exn in
      let good_ones =
        List.filter ns ~f:(fun (s, (_, d)) -> not (equal_dir d best_neighbor_dir) && s - 1000 = best_neighbor_score)
        |> List.map ~f:snd
      in
      inner ((inner [] good_ones) @ res) xs
    in
  in  *)
  Min_heap.add openSet { pos = start; dir = Right; path = [(start, Right)] } ~score:0;
  while Min_heap.length openSet > 0 do
    match Min_heap.pop_min_elt openSet with
    | None -> raise(invalid_arg "no path found")
    | Some((score, { pos; dir; path })) when Coord.equal pos finish -> begin
      let diff = if equal_dir dir Down then 0 else 0 in
      match Hashtbl.find seen pos with
      | None ->
        paths := path @ !paths;
        Hashtbl.add_exn seen ~key:pos ~data:(score + diff);
        Min_heap.empty openSet (* end early *)
      | _ -> ()
    end
    | Some((score, { pos; dir; path })) ->
      (try
          printf "next node is "; Coord._print pos; printf " with score %d" score;
          let v = Hashtbl.find_exn seen pos
          in
          printf " already seen with score=%d\n" v
      with
      | _ -> print_endline "");
      if not (Hashtbl.mem seen pos) then begin
        neighbors walls pos dir |> List.iter ~f:(fun (ns, (p, d)) ->
          printf "\t neighbor "; Coord._print p; printf " with score %d\n" (score + ns);
          Min_heap.add openSet { pos = p; dir = d; path = ((p, d) :: path) } ~score:(score + ns)
        );
        Hashtbl.add_exn seen ~key:pos ~data:score
      end
  done;
  (Hashtbl.find_exn seen finish, !paths)

let _debug walls chairs mi mj =
  for i = 0 to mi do
    for j = 0 to mj do
      let pos = (i, j) in
      if Set.mem walls pos then print_string "#"
      else if List.mem chairs pos ~equal:Coord.equal then print_string "0"
      else print_string "."
    done;
    print_endline ""
  done

(* let _go_back walls start start_dir finish =
  let rec inner res = function
  | [] -> res
  | (n, { pos; path; _ }) :: xs when n = 0 && Coord.equal pos finish -> inner (path @ res) xs
  | (n, _) :: xs when n = 0 -> inner res xs
  | (n, { pos; dir; path }) :: xs ->
    let l = neighbors walls pos dir |> List.map ~f:(fun (ns, (p, d)) -> (n - ns, { pos = p; dir = d; path = pos :: path })) in
    inner res (l @ xs)
  in
  inner [] [(7036, { pos = start; dir = start_dir ; path = [] })]
  |> Set.of_list (module Coord)
  |> Set.length *)

let () =
  (* let heap = Aoc_2024.Min_heap.create ~size:64 ~init:0 in
  let rec inner h = match (Aoc_2024.Min_heap.pop_min_elt h) with
  | None -> ()
  | Some((_, e)) -> printf "%d\n" e;
    inner h
  in
  List.iter [1;2;3;17;19;36;7;25;100] ~f:(fun x -> Aoc_2024.Min_heap.add heap x ~score: x);
  inner heap *)
  let walls = parse_input in
  let part1, part2 = find_path walls (139, 1) (1, 139) in
  printf "part1=%d\n" part1;
  printf "part2=%d\n" (List.length part2);

  _debug walls (List.map part2 ~f:fst) 140 140


  (* Set.iter part2 ~f:(fun pos -> Coord._print pos); *)
  (* printf "o=%d\n" (Hashtbl.find_exn part2 (7, 5));

  printf "down=%d\n" (Hashtbl.find_exn part2 (8, 5));
  printf "left=%d\n" (Hashtbl.find_exn part2 (7, 4)); *)




