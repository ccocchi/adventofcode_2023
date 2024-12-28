open Core

let parse_input =
  In_channel.read_lines "/Users/chris/code/adventofcode_2023/2024/data/day23.txt"
  |> List.map ~f:(String.split ~on:'-')
  |> List.map ~f:(function | a :: b :: [] -> (a, b) | _ -> invalid_arg("bad input"))

(* let vertices l = List.concat l |> List.dedup_and_sort ~compare:String.compare *)

module String3 = struct
  module T = struct
    type t = string * string * string [@@deriving compare, sexp_of]
  end

  include T
  include Comparator.Make(T)
end

module Graph = struct
  type string_set = (string, String.comparator_witness) Set_intf.Set.t

  type t = {
    mutable nodes: int;
    graph: (string, string_set) Hashtbl_intf.Hashtbl.t
  }

  let create = { nodes = 0; graph = Hashtbl.create (module String) }

  let add_edge g (src, dst) =
    (match Hashtbl.find g.graph src with
    | None ->
      g.nodes <- g.nodes + 1;
      Hashtbl.add_exn g.graph ~key:src ~data:(Set.add (Set.empty (module String)) dst)
    | Some(s) -> Hashtbl.set g.graph ~key:src ~data:(Set.add s dst));
    match Hashtbl.find g.graph dst with
    | None ->
      g.nodes <- g.nodes + 1;
      Hashtbl.add_exn g.graph ~key:dst ~data:(Set.add (Set.empty (module String)) src)
    | Some(s) -> Hashtbl.set g.graph ~key:dst ~data:(Set.add s src)
end

let build_graph l =
  let g = Graph.create in
  List.iter l ~f:(fun edge -> Graph.add_edge g edge);
  g

let part1 (g: Graph.t) =
  let seen = Hashtbl.create (module String) in
  Hashtbl.keys g.graph
  |> List.map ~f:(fun str1 -> match Hashtbl.find g.graph str1 with
    | None -> []
    | Some(s1) ->
      Hashtbl.add_exn seen ~key:str1 ~data:1;
      let l1 = Set.to_list s1 |> List.filter ~f:(fun s -> Hashtbl.mem seen s |> not) in
      List.filter_map l1 ~f:(fun str2 -> match Hashtbl.find g.graph str2 with
      | None -> None
      | Some(s2) ->
        let s3 = Set.inter s1 s2 in
        if Set.length s3 = 0 then None
        else
          List.map (Set.to_list s3) ~f:(fun str3 ->
            let l = List.sort [str1; str2; str3] ~compare: String.compare in
            match l with
            | a :: b :: c :: [] -> (a, b, c)
            | _ -> invalid_arg("no")
          )
          |> Some
      )
      |> List.concat
  )
  |> List.concat
  |> Set.of_list (module String3)
  |> Set.filter ~f:(fun (a, b, c) -> let prefix = "t" in
    String.is_prefix a ~prefix || String.is_prefix b ~prefix || String.is_prefix c ~prefix
  )
  |> Set.length

let () =
  let input = parse_input in
  let g = build_graph input in
  let l = part1 g in
  printf "part1=%d\n" l
  (* (* print_endline "aq";
  Set.iter (Hashtbl.find_exn g.graph "aq") ~f:(fun s -> printf "  -> %s\n" s);
  print_endline "cg";
  Set.iter (Hashtbl.find_exn g.graph "cg") ~f:(fun s -> printf "  -> %s\n" s) *)


  Set.iter l ~f:(fun (a, b ,c) -> printf "(%s, %s, %s)\n" a b c) *)
