open Core
open Re

module Workflow = Map.Make (String)

module Rating = struct
  type t = {
    x: int;
    m: int;
    a: int;
    s: int
  }

  let get_field r c = match c with
  | 'x' -> r.x
  | 'm' -> r.m
  | 'a' -> r.a
  | 's' -> r.s
  | _ -> failwith "wrong field"

  let sum r = r.x + r.m + r.a + r.s

  let of_string str =
    let nums =
    [ "x"; "m"; "a"; "s" ]
    |> List.map ~f:(fun f ->
      let reg =
        Re.(seq [ str (f ^ "="); digit |> rep1 |> group ] |> compile)
      in
      Group.get (Re.exec reg str) 1 |> int_of_string)
    in
    {
      x = List.nth_exn nums 0;
      m = List.nth_exn nums 1;
      a = List.nth_exn nums 2;
      s = List.nth_exn nums 3;
    }
end

module Rule = struct
  type pred = True | Pred of char * (int -> int -> bool) * int
  type t = { dest: string; predicate: pred }

  let accepts rule rtg = match rule.predicate with
  | True -> true
  | Pred (f, fn, n) ->
    fn (Rating.get_field rtg f) n

  let of_string str =
    if (String.length str) <= 3 then { dest = str; predicate = True }
    else
      match String.split str ~on:':' with
      | [s; dest] ->
        let field = s.[0]
        and fn = if (Char.equal s.[1] '<') then ( < ) else ( > )
        and n = int_of_string (String.subo s ~pos: 2)
        in
        { dest; predicate = Pred (field, fn, n) }
      | _ -> failwith ("ill-formatted rule " ^ str)
end

module Interval = struct
  type t = { min: int; max: int }

  type possibility = {
    x: t;
    m: t;
    a: t;
    s: t
  }

  let init = {
    x = { min = 1; max = 4000 };
    m = { min = 1; max = 4000 };
    a = { min = 1; max = 4000 };
    s = { min = 1; max = 4000 };
  }

  let combinations poss =
    let count t = t.max - t.min + 1 in
    count poss.x * count poss.m * count poss.a * count poss.s

  let get_field p c = match c with
  | 'x' -> p.x
  | 'm' -> p.m
  | 'a' -> p.a
  | 's' -> p.s
  | _ -> failwith "wrong field"

  let masks fn n =
    if (phys_equal ( < ) fn) then
      ({ min = 1; max = n - 1 }, { min = n; max = 4000 })
    else
      ({ min = n + 1; max = 4000 }, { min = 1; max = n })

  let apply_mask itvl mask =
    if (itvl.min > mask.max || itvl.max < mask.min) then None
    else Some({ min = Int.max itvl.min mask.min; max = Int.min itvl.max mask.max })

  let transform poss k v = match k with
  | 'x' -> { poss with x = v }
  | 'm' -> { poss with m = v }
  | 'a' -> { poss with a = v }
  | 's' -> { poss with s = v }
  | _ -> failwith "wrong transform"

  let go_through (rule: Rule.t) poss = match rule.predicate with
  | True -> (Some(poss), None)
  | Pred (f, fn, n) ->
    let field = get_field poss f
    and pass, fail = masks fn n
    and func = transform poss f in
    (
      Option.map (apply_mask pass field) ~f:func,
      Option.map (apply_mask fail field) ~f:func
    )
end

let process (workflow: Rule.t list Workflow.t) rtg =
  let rec inner = function
  | "A" -> true
  | "R" -> false
  | name ->
    let rules = Map.find_exn workflow name in
    let matching = List.find_exn rules ~f:(fun r -> Rule.accepts r rtg)
    in
    inner matching.dest
  in
  inner "in"

let part1 workflow ratings =
  List.filter ratings ~f:(process workflow)
  |> List.fold ~init:0 ~f:(fun acc r -> acc + Rating.sum r)

let part2 (workflow: Rule.t list Workflow.t) =
  let q = Queue.create ~capacity:(Map.length workflow) () in
  let rec inner acc = match Queue.dequeue q with
  | None -> acc
  | Some((poss, at)) -> match at with
    | "A" -> inner (poss :: acc)
    | "R" -> inner acc
    | _ ->
      let rules = Map.find_exn workflow at in
      let _ = List.fold_until rules ~init:poss ~finish:(fun _ -> 1)
        ~f:(fun p rule ->
          let pass, cnt = Interval.go_through rule p in
          (match pass with
          | Some(np) -> Queue.enqueue q (np, rule.dest)
          | None -> ());
          match cnt with
          | None -> Stop(1)
          | Some(np) -> Continue(np)
      )
      in
      inner acc
  in
  Queue.enqueue q (Interval.init, "in");
  inner [] |> List.fold ~init:0 ~f:(fun acc p -> acc + Interval.combinations p)

let parse_step str =
  let i = String.index_exn str '{' in
  let name = String.sub str ~pos:0 ~len: i
  and r = String.sub str ~pos:(i + 1) ~len:((String.length str) - i - 2)
  in
  (name, String.split r ~on: ',' |> List.map ~f: Rule.of_string)

let parse_input =
  In_channel.read_lines "/Users/chris/code/adventofcode_2023/data/day19.txt"
  |> List.group ~break:(fun l1 l2 -> String.is_empty l1 || String.is_empty l2)
  |> List.filter ~f:(fun l -> List.for_all l ~f:(String.equal "") |> not)
  |> function
    | [steps; ratings] -> (List.map steps ~f: parse_step, List.map ratings ~f: Rating.of_string)
    | _ -> failwith "wrong input format"

let () =
  let steps, ratings = parse_input in
  let workflow = Workflow.of_alist_exn steps in
  printf "part1=%d\n" (part1 workflow ratings);
  printf "part2=%d\n" (part2 workflow)
