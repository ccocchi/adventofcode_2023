open Core

let initialize_wires =
  let table = Hashtbl.create (module String) in
  In_channel.read_lines "/Users/chris/code/adventofcode_2023/2024/data/day24_1.txt"
  |> List.map ~f:(String.split ~on:':')
  |> List.iter ~f:(function
    | key :: v :: [] ->
      Hashtbl.add_exn table ~key ~data:(int_of_string (String.strip v))
    | _ -> invalid_arg("bad input"));
  table

exception Stop

module Op = struct
  type op = And | Or | Xor [@@deriving equal]
  type t = (string * op * string * string)

  type tree =
  | Leaf of string
  | Node of tree * op * tree

  let of_string = function
  | "AND" -> And
  | "OR" -> Or
  | "XOR" -> Xor
  | _ -> invalid_arg("bad op")

  let to_string = function
  | And -> "AND"
  | Or -> "OR"
  | Xor -> "XOR"

  let table_from_list (l: t list) =
    let table = Hashtbl.create (module String) ~size:256 in
    List.iter l ~f:(fun (s1, op, s2, res) -> Hashtbl.add_exn table ~key:res ~data:(s1, op, s2));
    table

  let is o t x y = match t with
    | Node(Leaf(a), op, Leaf(b)) when equal_op o op ->
      (String.equal a x && String.equal b y) || (String.equal a y && String.equal b x)
    | _ -> false

  let is_xor = is Xor
  let is_and = is And
  (* let is_or = is Or *)
end

let parse_input: Op.t list =
  let r = Re.compile (Re.Posix.re "(.{3}) ([A-Z]{2,3}) (.{3}) -> (.{3})") in
  In_channel.read_lines "/Users/chris/code/adventofcode_2023/2024/data/day24_2.txt"
  |> List.map ~f:(fun s -> Re.exec ~pos:0 r s)
  |> List.map ~f:(fun g ->
    (Re.Group.get g 1, Op.of_string (Re.Group.get g 2), Re.Group.get g 3, Re.Group.get g 4)
  )

let part1 table input =
  let rec inner n = function
  | [] -> if (List.length n) > 0 then inner [] n else ()
  | x :: xs -> try
    let (l, op, r, wire) = x in
    let v1 = Hashtbl.find_exn table l and v2 = Hashtbl.find_exn table r in
    begin
      let res = match op with
      | Op.And -> Int.bit_and v1 v2
      | Op.Or -> Int.bit_or v1 v2
      | Op.Xor -> Int.bit_xor v1 v2
      in
      Hashtbl.add_exn table ~key:wire ~data:res;
      inner n xs
    end
  with
  | Not_found_s(_) -> inner (x :: n) xs
  in
  inner [] input;
  Hashtbl.filter_keys table ~f:(String.is_prefix ~prefix:"z")
  |> Hashtbl.keys
  |> List.sort ~compare:String.compare
  |> List.foldi ~init:0 ~f:(fun i acc s ->
    if Hashtbl.find_exn table s > 0 then acc + Int.pow 2 i else acc
  )

let debug input s =
  let rtable = Op.table_from_list input in
  let expand str =
    let rec build_tree str = match Hashtbl.find rtable str with
    | None -> Op.Leaf(str)
    | Some((s1, op, s2)) ->
      (* printf "%s - %s\n" s1 s2; *)
      Op.Node(build_tree s1, op, build_tree s2)
    and _print_tree = function
    | Op.Node(l, op, r) ->
      print_string "(";
      _print_tree l;
      printf " %s " (Op.to_string op);
      _print_tree r;
      print_string ")"
    | Op.Leaf(w1) -> print_string w1
    in
    _print_tree (build_tree str);
    print_endline ""
  in
  expand s

let _part2 input =
  let max = 44 in
  let rec inner res = function
  | n when n = max -> ()
  | n ->
    let table = Hashtbl.create ~size:64 (module String) in
    for i = 0 to max do
      let s = String.pad_left ~char:'0' ~len:2 (string_of_int i)
      and v = if i <= n then 1 else 0 in
      Hashtbl.add_exn table ~key:("x" ^ s) ~data: v;
      Hashtbl.add_exn table ~key:("y" ^ s) ~data: v
    done;
    let _ = part1 table input in
    (* printf "n=%d sum=%d expected=%d\n" n sum (2 * ((Int.pow 2 (n + 1)) - 1)); *)
    inner res (n + 1)
  in
  inner [] 0

let make_var s i = s ^ String.pad_left ~char:'0' ~len:2 (string_of_int i)

let rec tree_is_carry t n =
  let x = make_var "x" n and y = make_var "y" n in
  match n with
  | 0 -> Op.is_and t x y
  | _ -> begin
    match t with
    | Op.Node(a, Op.Or, b) -> begin
      try
        let node = if Op.is_and a x y then b
        else if Op.is_and b x y then a
        else raise(Stop)
        in
        match node with
        | Op.Node(a', Op.And, b') when Op.is_xor a' x y -> tree_is_carry b' (n-1)
        | Op.Node(a', Op.And, b') when Op.is_xor b' x y -> tree_is_carry a' (n-1)
        | _ -> false
      with
      | Stop -> false
    end
    | _ -> false
  end

let check_bit input n =
  let x = make_var "x" n and y = make_var "y" n and z = make_var "z" n in
  let rtable = Op.table_from_list input in
  let rec build_tree str = match Hashtbl.find rtable str with
  | None -> Op.Leaf(str)
  | Some((s1, op, s2)) -> Op.Node(build_tree s1, op, build_tree s2)
  in
  let t = build_tree z in
  match n with
  | 0 -> Op.is_xor t x y
  | _ ->
    match t with
    | Op.Node(left, Op.Xor, right) when Op.is_xor left x y -> tree_is_carry right (n-1)
    | Op.Node(left, Op.Xor, right) when Op.is_xor right x y -> tree_is_carry left (n-1)
    | _ -> printf "fail fast\n"; false

let () =
  let table = initialize_wires and input = parse_input in
  printf "part1=%d\n" (part1 table input);
  (* debug input "z01";
  debug input "z02";
  debug input "z03" *)
  debug input "z19";
  for i = 0 to 44 do
    printf "i=%d valid=%b\n" i (check_bit input i)
  done
  (* printf "valid=%b\n" (check_bit input 19) *)

