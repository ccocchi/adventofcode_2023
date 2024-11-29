open Core

module Card = struct
  type card = {
    mutable attack: int;
    mutable health: int;
    mutable maxAttack: int;
    mutable maxHealth: int;
    mutable taunt: bool;
    mutable divineShield: bool;
    mutable venom: bool;
    poison: bool;
  }

  let hasTaunt c = c.taunt

  let resolveAttack (attacker: card) (defenser: card) =
    defenser.health <- (defenser.health - attacker.attack);
    attacker.health <- (attacker.health - defenser.attack)

  let randomPick l =
      let nd = List.map ~f:(fun c -> (Random.bits (), c)) l in
      let sond = List.sort ~compare:(fun (a, _) (b, _) -> Int.compare a b) nd in
      List.hd_exn sond |> snd

  let findTarget (ary: card list): card = match ary with
    | c :: [] -> c
    | [] -> failwith("no target")
    | _ -> match List.filter ~f:hasTaunt ary with
      | c :: [] -> c
      | [] -> randomPick ary
      | t -> randomPick t

  let round ((p1: card list), (p2: card list)) = match p1 with
  | c :: _ ->
    resolveAttack c (findTarget p2)
  | _ -> failwith("meh")

  let fight (p1: card list) (p2: card list) = match (p1, p2) with
  | ([], []) -> 0
  | ([], _) -> 1
  | (_, []) -> -1
end






type dir = X | Y | Z
type space = int array array array

module Coord = struct
  type coord = {
    x: int;
    y: int;
    z: int
  }

  let of_string s = match String.split s ~on:',' |> List.map ~f:int_of_string with
  | [x; y; z] -> { x; y; z }
  | _ -> failwith "wrong coordinate"
end

module Vector = struct
  include Coord

  type t = coord * coord * dir

  let of_string s: t =
    let dir (c1: coord) (c2: coord) =
      if not (Int.equal c1.x c2.x) then X
      else if not (Int.equal c1.y c2.y) then Y
      else Z
    in
    String.split s ~on: '~' |> List.map ~f: Coord.of_string |> function
  | [s; e] -> (s, e, dir s e)
  | _ -> failwith "wrong vector"

  let translateZ (v: t) (n: int): t =
    if Int.equal 0 n then v
    else
      let c1, c2, dir = v in
      ({ c1 with z = c1.z + n }, { c2 with z = c2.z + n}, dir)

  let minZ (v: t) =
    let c1, c2, _ = v in Int.min c1.z c2.z

  let maxZ (v: t) =
    let c1, c2, _ = v in Int.max c1.z c2.z

  let compareZ ((c1, c2, _): t) ((c'1, c'2, _): t) =
    Int.compare (Int.min c1.z c2.z) (Int.min c'1.z c'2.z)

  let pointsXY ((c1, c2, dir): t) = match dir with
  | X -> List.init (c2.x - c1.x + 1) ~f: (fun x -> (c1.x + x, c1.y))
  | Y -> List.init (c2.y - c1.y + 1) ~f: (fun y -> (c1.x, c1.y + y))
  | Z -> [(c1.x, c1.y)]

  let _print (c1, c2, _) = printf "(%d, %d, %d)~(%d, %d, %d)\n" c1.x c1.y c1.z c2.x c2.y c2.z
end

let read_lines =
  In_channel.read_lines "/Users/chris/code/adventofcode_2023/data/day22.txt"
  |> List.map ~f:Vector.of_string
  |> List.sort ~compare:Vector.compareZ

let gravity (ary: space) (v, label) =
  let points = Vector.pointsXY v in
  let minz = (Vector.minZ v) in
  let fall = Array.sub ary ~pos:0 ~len: (minz - 1)
  |> Array.rev
  |> Array.fold_until ~init:0 ~finish:(fun x -> x) ~f:(fun acc flat ->
    if List.for_all points ~f:(fun (x, y) -> Int.equal 0 flat.(x).(y)) then Continue(acc + 1)
    else
      Stop(acc)
  )
  in
  (* List.iter points ~f:(fun (x, y) -> printf "point=%d,%d\n" x y); *)
  let tv = Vector.translateZ v (0 - fall) in
  for z = (Vector.minZ tv) - 1 to (Vector.maxZ tv) - 1 do
    List.iter points ~f:(fun (x, y) -> ary.(z).(x).(y) <- label)
  done;
  (* Vector._print tv; *)
  tv

let find_supports ary v =
  let z = Vector.minZ v in
  if Int.equal 1 z then []
  else
  Vector.pointsXY v
  |> List.map ~f:(fun (x, y) -> ary.(z - 2).(x).(y))
  |> List.filter ~f:(fun i -> i > 0)
  |> Set.of_list (module Int)
  |> Set.to_list

let part1 supports =
  let seq = Sequence.init (List.length supports) ~f:(fun x -> x + 1)
  in
  supports
  |> List.fold ~init:(Set.of_sequence (module Int) seq) ~f:(fun acc (_, l) ->
    match l with
    | hd :: [] -> Set.remove acc hd
    | _ -> acc
  )
  |> fun set -> printf "part1=%d\n" (Set.length set)

let remove_all els ~from =
  List.filter from ~f:(fun i -> List.mem els i ~equal: Int.equal |> not)

let part2 supports =
  let start = List.filter supports ~f:(fun (_, l) -> not (List.is_empty l)) in
  let rec disintegrate l res bricks =
    let boom, rest = List.partition_map l ~f:(fun (lbl, nl) ->
      let list_without_bricks = remove_all bricks ~from:nl in
      if List.is_empty list_without_bricks then First lbl
      else Second (lbl, list_without_bricks)
    )
    in match boom with
    | [] -> res
    | _ -> disintegrate rest (res + List.length boom) boom
  in
  List.init (List.length supports) ~f:(fun x -> x + 1)
  |> List.map ~f:(fun removed -> disintegrate start 0 [removed])
  |> List.fold ~init:0 ~f:( + )
  |> printf "part2=%d\n"

let () =
  let vectors = read_lines in
  let ary = Array.init 500 ~f:(fun _ -> Array.make_matrix ~dimx:10 ~dimy: 10 0)
  in
  (* List.iter vectors ~f:Vector._print; *)
  let supports =
    vectors
    |> List.mapi ~f:(fun i v ->
      let nv = gravity ary (v, i + 1) in (i + 1, find_supports ary nv)
    )
  in
    (* List.iteri supports ~f:(fun i l ->
      printf "%d supported by %s\n" (i + 1) (List.to_string l ~f:string_of_int)
    ); *)
    part1 supports;
    part2 supports

  (* |> List.iter ~f:(fun l -> printf "%s\n" (List.to_string l ~f:string_of_int)) *)


