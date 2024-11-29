open Core

module ConfigMap = Map.Make (String)
module StateMap = Map.Make (String)

type fq = Low | High
type pulse = {
  from: string;
  target: string;
  frequency: fq
}
type amodule = FlipFlop | Conjuction | Broadcaster

type st = {
  mutable is_on: bool;
  mutable inputs: (string * fq) list
}

let _freq_to_string = function
| Low -> "low"
| High -> "high"

let parse_input () =
  let parse_type = function
  | '%' -> FlipFlop
  | '&' -> Conjuction
  | _ -> Broadcaster
  and parse_value s = String.split s ~on:',' |> List.map ~f:String.lstrip in
  In_channel.read_lines "/Users/chris/code/adventofcode_2023/data/day20.txt"
  |> List.map ~f:(fun l -> match l.[0] with
  | '%' | '&' ->
    let key = String.sub l ~pos:1 ~len:2
    and typ = l.[0]
    and v = String.subo l ~pos:7
    in
    (key, (parse_type typ, parse_value v))
  | _ ->
    let v = String.subo l ~pos:15 in
    ("broadcaster", (Broadcaster, parse_value v))
  )

let process (cmap: (amodule * string list) ConfigMap.t) smap (p: pulse) =
  let at = p.target in
  let make_signal frequency target = { from = at; target; frequency } in
  match Map.find cmap at with
  | None -> (smap, [])
  | Some(typ, children) ->
    match (p.frequency, typ) with
    | (_, Broadcaster) -> (smap, List.map children ~f:(make_signal Low))
    | (High, FlipFlop) -> (smap, [])
    | (Low, FlipFlop) -> (
      match Map.find smap at with
      | Some(st) when st.is_on ->
        st.is_on <- false;
        (smap, List.map children ~f:(make_signal Low))
      | Some(st) ->
        st.is_on <- true;
        (smap, List.map children ~f:(make_signal High))
      | None ->
        let m = Map.add_exn smap ~key:at ~data:{is_on = true; inputs = []} in
        (m, List.map children ~f:(make_signal High))
    )
    | (s, Conjuction) ->
      match Map.find smap at with
      | None -> failwith "conjunction without state"
      | Some(st) ->
        st.inputs <- List.map st.inputs ~f:(fun (name, t) -> if String.equal name p.from then (name, s) else (name, t));
        let go_low = List.for_all st.inputs ~f:(fun (_, freq) -> match freq with | High -> true | Low -> false ) in
        (smap, List.map children ~f:(make_signal (if go_low then Low else High)))

let initialize_map_with_conjuctions (cmap: (amodule * string list) ConfigMap.t) =
  Map.filter cmap ~f:(fun x -> match x with | (Conjuction, _) -> true | _ -> false)
  |> Map.keys
  |> List.map ~f:(fun key ->
    let connected = Map.filter cmap ~f:(fun (_, l) -> List.mem l key ~equal:String.equal) |> Map.keys in
    let inputs = List.map connected ~f:(fun c -> (c, Low)) in
    (key, { is_on = false; inputs })
  )
  |> StateMap.of_alist_exn

let () =
  let input = parse_input () in
  let cmap = ConfigMap.of_alist_exn input in
  let lcnt = ref 0 and hcnt = ref 0 in
  let rec inner smap pulses = match pulses with
  | [] -> Some(smap)
  | p :: tail ->
    (* Cheesing part2 like a boss *)
    (* xn qn xf zl needs to send a High to `th` in order for `rc` to receive a Low so we can assume the puzzle *)
    (* is not sneaky and a simple LCM on these four input will do the trick *)
    if (String.equal "zl" p.from && String.equal "th" p.target && match p.frequency with | High -> true | _ -> false)
    then
      None
    else begin
      (* printf "%s -%s-> %s\n" p.from (_freq_to_string p.frequency) p.target; *)
      (match p.frequency with | Low -> incr lcnt | _ -> incr hcnt);
      let m, ps = process cmap smap p in
      inner m (tail @ ps)
    end
  in
  let smap = initialize_map_with_conjuctions cmap in
  let i = ref 0 in
  let seq = Sequence.unfold_step ~init:smap ~f:(fun s ->
    incr i;
    match inner s [{ from = "button"; target = "broadcaster"; frequency = Low }] with
    | Some(new_s) ->
      Yield { value = new_s; state = new_s }
    | None -> Done
    )
  in
    let _ = Sequence.nth_exn seq 999 in
    printf "part1=%d\n" (!lcnt * !hcnt);
    printf "part2=%d\n" (List.fold_left [4027;3793;3923;3739] ~init:1 ~f:Aoc.Utils.lcm)
