open Core

let parse_input =
  In_channel.read_all "/Users/chris/code/adventofcode_2023/2024/data/day9.txt"
  |> String.strip
  |> String.to_list
  |> List.map ~f:(fun x -> Int.of_string (Char.to_string x))

type memory = File | Free [@@deriving eq]

type block = {
  file_id: int;
  size: int;
  mode: memory;
}

let _print_block ({mode; size; file_id}) =
  let s = match mode with
  | File -> "file"
  | Free -> "free"
  in
  printf "(%s of size %d with id %d)\n" s size file_id

let populate_memory ary input = List.fold input ~init:(0, 0, File) ~f:(fun (curr, n, mode) x ->
  match mode with
  | File ->
    for i = curr to curr + x - 1 do
      ary.(i) <- n
    done;
    (curr + x, n + 1, Free)
  | Free -> (curr + x, n, File)
)
|> fun _ -> ()

let compact ary =
  let st = ref 0 and nd = ref ((Array.length ary) - 1) in
  while !st < !nd do
    while ary.(!st) >= 0 do incr st done;
    while ary.(!nd) < 0 do decr nd done;

    if !st < !nd then Array.swap ary !st !nd
  done

let free_block b list =
  let rec inner acc = function
  | [] -> acc
  | x :: xs when Int.equal x.file_id b.file_id -> inner ({ size = x.size; file_id = -1; mode = Free } :: acc) xs
  | x :: xs -> inner (x :: acc) xs
in
  List.rev (inner [] list)

let move_block b list =
  let rec inner acc l = match l with
  | [] -> list
  | x :: _ when x.file_id = b.file_id -> list
  | x :: xs when equal_memory x.mode File -> inner (x :: acc) xs
  | x :: xs when Int.equal x.size b.size -> (List.rev acc) @ b :: (free_block b xs)
  | x :: xs when x.size < b.size -> inner (x :: acc) xs
  | x :: xs ->
    let nf = { file_id = -1; size = x.size - b.size; mode = Free } in
    (List.rev acc) @ b :: nf :: (free_block b xs)
in
  inner [] list

let whole_compact list =
  let files = List.filter list ~f:(fun x -> equal_memory x.mode File) |> List.rev in
  List.fold files ~init:list ~f:(fun acc b -> move_block b acc)

let make_blocks_list input =
  let rec inner acc n mode = function
  | [] -> acc
  | x :: xs ->
    match mode with
    | File -> inner ({ file_id = n; size = x; mode } :: acc) (n + 1) Free xs
    | Free -> inner ({ file_id = -1; size = x; mode } :: acc) n File xs
in
  List.rev (inner [] 0 File input)

let checksum l =
  let rec inner acc n = function
  | [] -> acc
  | x :: xs when x < 0 -> inner acc (n + 1) xs
  | x :: xs -> inner (acc + n * x) (n + 1) xs
in
  inner 0 0 l

let checksumb l =
  let rec inner acc n = function
  | [] -> acc
  | x :: xs when equal_memory Free x.mode -> inner acc (n + x.size) xs
  | x :: xs ->
    let sum =
      List.init x.size ~f:(fun i -> n + i)
      |> List.fold ~init:Int.zero ~f:(fun acc i -> acc + i * x.file_id)
    in
    inner (acc + sum) (n + x.size) xs
  in
  inner 0 0 l

let _dump ary =
  Array.iter ary ~f:(fun x -> if (x < 0) then print_string "." else printf "%d" x);
  print_string "\n"

let _dumpb l =
  List.iter l ~f:(fun b ->
    for _ = 0 to b.size - 1 do
      match b.mode with
      | Free -> print_string "."
      | File -> printf "%d" b.file_id
    done
  );
  print_string "\n"

let part1 input =
  let length = List.reduce_exn input ~f:((+)) in
  let ary = Array.create ~len:length (-1) in
    populate_memory ary input;
    compact ary;
    checksum (Array.to_list ary)

let part2 input = checksumb ((whole_compact (make_blocks_list input)))

let () =
  let input = parse_input
in
  printf "part1=%d\n" (part1 input);
  printf "part2=%d\n" (part2 input)


