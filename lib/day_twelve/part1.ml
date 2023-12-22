(* Johnson Trotter algorithm *)
type direction = L | R
type element = char * int * direction

let find_largest_movable_element a =
  let l = Array.length a in
  let can_move e i = match e with
  | (_, _, L) when i == 0 -> false
  | (_, _, R) when i >= l - 1 -> false
  | (_, n, L) ->
    let (_, x, _) = Array.unsafe_get a (i - 1) in n > x
  | (_, n, R) ->
    let (_, x, _) = Array.unsafe_get a (i + 1) in n > x
  in
  let rec inner acc m i =
    let curr = Array.unsafe_get a i in
    if (i == l) then acc
    else
      if (not (can_move curr i)) then inner acc m (i + 1)
      else
        let (_, n, _) = curr in
        match m with
        | None -> inner (Some(i)) (Some(n)) (i + 1)
        | Some(max) -> if (n > max) then inner (Some(i)) (Some(n)) (i + 1) else inner acc m (i + 1)
  in
  inner None None 0

let move ary i =
  let swap x y =
    let tmp = ary.(x)
    in
      ary.(x) <- ary.(y);
      ary.(y) <- tmp;
      tmp
  in
  match Array.unsafe_get ary i with
  | (_, _, L) -> swap i (i - 1)
  | (_, _, R) -> swap i (i + 1)

let flip_scan ary (_, threshold, _) =
  let flip = function | L -> R | R -> L in
  (* printf "threshold = %i\n" threshold; *)
  Array.iteri (fun i (c, x, d) -> if (x > threshold) then Array.unsafe_set ary i (c, x, flip d)) ary


let bytes_to_array str =
  let ary = Array.make (Bytes.length str) '?' in
  Bytes.iteri (fun i c -> Array.unsafe_set ary i c) str;
  ary

let generate_permutations str =
  let l = Bytes.length str in
  let prepared = Array.mapi (fun i x -> (x, i, L)) (bytes_to_array str) in
  let screenshot () =
    let bb = Bytes.init l (fun i -> let (c, _, _) = Array.unsafe_get prepared i in c) in
    (* print_string "permutation = ";
    print_bytes bb;
    print_newline();
    print_newline(); *)
    bb
  in
  let rec inner acc = match find_largest_movable_element prepared with
  | None -> screenshot() :: acc
  | Some(i) ->
    (* printf "premove with i=%i\n" i;
    Array.iter (fun (x, _, _) -> print_char x; print_char ' ') prepared;
    print_newline();
    Array.iter (fun (_, w, _) -> print_int w; print_char ' ') prepared;
    print_newline();
    Array.iter (fun (_, _, d) -> print_char (if d == L then 'L' else 'R'); print_char ' ') prepared;
    print_newline(); *)
    flip_scan prepared (move prepared i);
    inner (screenshot() :: acc)
  in
  inner [screenshot()]

let count_chars str = String.fold_left (fun (l, r) x -> match x with
  | '?' -> (l + 1, r)
  | '#' -> (l, r + 1)
  | _ -> (l, r)
) (0, 0) str

let generate_first_string width to_fix =
  let b = Bytes.make width '.' in
  (* Printf.printf "width=%i to_fix=%i" width to_fix; *)
  for i = 0 to to_fix - 1 do
    Bytes.unsafe_set b i '#'
  done;
  b

let does_match_record record b =
  let l = Bytes.length b in
  let rec inner acc curr i =
    if (i < 0) then match curr with
      | None -> acc
      | Some(r) -> r :: acc
    else match (Bytes.unsafe_get b i, curr) with
      | ('.', None) -> inner acc curr (i - 1)
      | ('.', Some(r)) -> inner (r :: acc) None (i - 1)
      | (_, None) -> inner acc (Some(1)) (i - 1)
      | (_, Some(r)) -> inner acc (Some(r + 1)) (i - 1)
  in
  let l = inner [] None (l - 1) in
  (* Printf.printf "trying with %s\n" (Bytes.to_string b); *)
  List.equal Int.equal l record

let apply_permutation b str =
  let res = Bytes.of_string str
  and j = ref 0
  in
  Bytes.iteri (fun i c ->
    if c == '?' then begin
      Bytes.unsafe_set res i (Bytes.get b !j);
      incr j
    end
  ) res;
  res

let find_possible_arrangements gears record =
  (* let (gears, record) = ("?###????????", [3; 2; 1]) in *)
  let (broken, ok) = count_chars gears in
  let missing = (Batteries.List.sum record) - ok in
  let initial = generate_first_string broken missing in
  let permutations = Batteries.List.unique ~eq: Bytes.equal (generate_permutations initial) in

  permutations |> List.filter (fun x -> apply_permutation x gears |> does_match_record record) |> List.length

let res () =
  let str = Utils.read_whole_file "/Users/chris/code/adventofcode_2023/data/day_twelve.txt" in
  let lines = String.split_on_char '\n' str in
  List.fold_left (fun acc l ->
    let (gears, records) = String.split_on_char ' ' l |> Utils.list_to_tuple in
    acc + (find_possible_arrangements gears ((String.split_on_char ',' records) |> List.map int_of_string))
  ) 0 (List.filter (fun x -> String.length x > 1 ) lines)



