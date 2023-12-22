open Core

let read_lines =
  let orig =
    In_channel.read_lines "/Users/chris/code/adventofcode_2023/data/day14.txt"
    |> List.map ~f:String.to_array
    |> Array.of_list
  in
  let n, m = (Array.length orig, Array.length orig.(0)) in
  let trans = Array.make_matrix ~dimx:m ~dimy:n '.' in
  for i = 0 to n - 1 do
    for j = 0 to m - 1 do
      trans.(j).(i) <- orig.(i).(j)
    done
  done;
  (orig, trans)

type direction = N | W | S | E

let _debug mat =
  let n, m = (Array.length mat, Array.length mat.(0)) in
  for j = 0 to n - 1 do
    for i = 0 to m - 1 do
      printf "%c" mat.(j).(i)
    done;
    Out_channel.newline stdout;
  done;
  Out_channel.newline stdout

let tilt mat dir =
  let n, m = (Array.length mat, Array.length mat.(0))
  and cursor = ref 0
  in
  match dir with
  | N ->
    for i = 0 to m - 1 do
      for j = 0 to n - 1 do
        match mat.(j).(i) with
        | 'O' when !cursor = j -> incr cursor
        | 'O' ->
          mat.(!cursor).(i) <- 'O';
          mat.(j).(i) <- '.';
          incr cursor
        | '#' ->
          cursor := (j + 1)
        | _ -> ()
      done;
      cursor := 0
    done
  | W ->
    for j = 0 to n - 1 do
      for i = 0 to m - 1 do
        match mat.(j).(i) with
        | 'O' when !cursor = i -> incr cursor
        | 'O' ->
          mat.(j).(!cursor) <- 'O';
          mat.(j).(i) <- '.';
          incr cursor
        | '#' ->
          cursor := (i + 1)
        | _ -> ()
      done;
      cursor := 0
    done
  | S ->
    for i = m - 1 downto 0 do
      cursor := n - 1;
      for j = n - 1 downto 0 do
        match mat.(j).(i) with
        | 'O' when !cursor = j -> decr cursor
        | 'O' ->
          mat.(!cursor).(i) <- 'O';
          mat.(j).(i) <- '.';
          decr cursor
        | '#' ->
          cursor := (j - 1)
        | _ -> ()
      done;
    done
  | E ->
    for j = n - 1 downto 0 do
      cursor := m - 1;
      for i = m - 1 downto 0 do
        match mat.(j).(i) with
        | 'O' when !cursor = i -> decr cursor
        | 'O' ->
          mat.(j).(!cursor) <- 'O';
          mat.(j).(i) <- '.';
          decr cursor
        | '#' ->
          cursor := (i - 1)
        | _ -> ()
      done;
    done

let cycle mat =[N; W; S; E] |> List.iter ~f:(tilt mat)

let sum_load mat =
  let m = Array.length mat.(0) in
  Array.foldi mat ~init:0 ~f:(fun i acc r ->
      acc + ((m - i) * Array.count r ~f:(Char.equal 'O')))

let sum_load_without_moving col =
  let h = Array.length col in
  let f = Array.fold ~init:(0, h, 0) ~f:(fun (sum, v, obs) -> function
  | '.' -> (sum, v, obs + 1)
  | 'O' -> (sum + v, v - 1, obs)
  | _ -> (sum, v - obs - 1, 0)
  )
  in
  let (weights, _, _) = f col in
  weights

let snapshot mat =
  let n, m = (Array.length mat, Array.length mat.(0)) in
  let rec inner i j acc =
    if (j < 0) then acc
    else if (i < 0) then inner (m - 1) (j - 1) acc
    else
      match mat.(j).(i) with
      | 'O' -> inner (i - 1) j (j * m + i :: acc)
      | _ -> inner (i - 1) j acc
  in
  inner (m - 1) (n - 1) []

let part2 mat =
  let t = Hashtbl.create (module Int)
  and numbers = Sequence.unfold_step ~init:0 ~f:(fun n -> Yield { value = n + 1; state = n + 1})
  and loop_start = ref 0 in
  let loop_end, _ = numbers |> Sequence.drop_while_option ~f:(fun i ->
    cycle mat;
    let key = sum_load mat
    and snap = snapshot mat
    in
    match Hashtbl.add t ~key:key ~data:(i, snap) with
    | `Duplicate ->
      let prev_id, prev_snap = Hashtbl.find_exn t key in
      if (List.equal Int.equal snap prev_snap) then (
        loop_start := prev_id;
        false
      ) else true
    | `Ok -> true
  ) |> Option.value_exn
  in
  let total_cycles = 1_000_000_000 in
  let loop_size = loop_end - !loop_start in
  let r = (total_cycles - loop_end) mod loop_size
  and inf = Sequence.repeat () in
  Sequence.take inf r |> Sequence.iter ~f:(fun _ -> cycle mat);
  sum_load mat

let () =
  let (orig, trans) = read_lines in
  let part1 = Array.fold trans ~init: 0 ~f:(fun acc col -> acc + (sum_load_without_moving col)) in
  printf "Part1=%d\nPart2=%d\n" part1 (part2 orig)

