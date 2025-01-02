open Core

let parse_input =
  In_channel.read_lines "/Users/chris/code/adventofcode_2023/2024/data/day23.txt"
  |> List.map ~f:(String.split ~on:'-')
  |> List.map ~f:(function | a :: b :: [] -> (a, b) | _ -> invalid_arg("bad input"))

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

let part2 (g: Graph.t) =
  let seen = Hashtbl.create (module String) in
  let rec inner res = function
  | [] -> res
  | x :: xs when (List.length x) = 1 ->
    let node = (List.hd_exn x) in
    let conns = Hashtbl.find_exn g.graph node |> Set.to_list |> List.map ~f:(fun e -> e :: x) in
    inner res (conns @ xs)
  | x :: xs ->
    let key = String.concat ~sep:"," (List.sort ~compare:String.compare x) in
    if Hashtbl.mem seen key then inner res xs
    else
      let r = if (List.length x) > (List.length res) then x else res
      and l =
        List.map x ~f:(Hashtbl.find_exn g.graph)
        |> List.reduce_exn ~f:Set.inter
        |> Set.to_list
        |> List.map ~f:(fun e -> e :: x)
      in
      Hashtbl.add_exn seen ~key ~data:1;
      inner r (l @ xs)
  in
  inner [] (Hashtbl.keys g.graph |> List.map ~f:(fun x -> [x]))
  |> List.sort ~compare:String.compare
  |> String.concat ~sep:","

let () =
  let input = parse_input in
  let g = build_graph input in
  printf "part1=%d\n" (part1 g);
  printf "part2=%s\n" (part2 g)
