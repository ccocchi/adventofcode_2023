open Core
(* open Core_bench *)

(* let () =
  let a = 100000000 and n = 16777216 and _ = 33554431 in
  printf "%d == %d\n" (a mod n) (Int.bit_and a 16777215);

  Command_unix.run (Bench.make_command [
    Bench.Test.create ~name:"mod"
      (fun () -> ignore (a mod n));
    Bench.Test.create ~name:"and"
      (fun () -> ignore (Int.bit_and a 16777215 ))
  ]) *)

module ChangeSequence = struct
  type t = (int * int * int * int) [@@deriving hash, compare, sexp_of]

  let to_string (a, b, c, d) = sprintf "%d, %d, %d, %d" a b c d
end

let parse_input =
  In_channel.read_lines "/Users/chris/code/adventofcode_2023/2024/data/day22.txt"
  |> List.map ~f:int_of_string

let generate_secret_number seed stretches =
  let rec inner acc = function
  | 0 -> acc
  | n ->
    let a = ((acc * 64) lxor acc) mod 16777216 in
    let b = ((a / 32) lxor a) mod 16777216 in
    let c = ((b * 2048) lxor b) mod 16777216 in
    inner c (n - 1)
  in
  inner seed stretches

let part1 input =
  List.map input ~f:(fun x -> generate_secret_number x 2000)
  |> List.fold ~init:0 ~f:(+)
  |> printf "part1=%d\n"

let generate_prices seed stretches =
  let rec inner acc number = function
  | 0 -> acc
  | n ->
    let a = ((number * 64) lxor number) mod 16777216 in
    let b = ((a / 32) lxor a) mod 16777216 in
    let c = ((b * 2048) lxor b) mod 16777216 in
    inner ((c mod 10) :: acc) c (n - 1)
  in
  inner [seed mod 10] seed stretches
  |> List.rev

let make_sequences prices =
  let table = Hashtbl.create (module ChangeSequence) in
  let rec inner = function
  | a :: b :: c ::d ::e :: tl ->
    let key = (b - a, c - b, d - c, e - d) in
    ignore (Hashtbl.add table ~key ~data:e);
    inner (b :: c ::d ::e :: tl)
  | _ -> ()
  in
  inner prices;
  table

let part2 (input: int list) =
  let f x = generate_prices x 2000 |> make_sequences in
  let tables = List.map input ~f in
  let sequences =
    List.map tables ~f:(fun t -> Hashtbl.keys t)
    |> List.concat
    |> List.dedup_and_sort ~compare:ChangeSequence.compare
  in
  let (seq, v) = List.map sequences ~f:(fun seq ->
    let bananas = List.fold tables ~init:0 ~f:(fun acc t -> match Hashtbl.find t seq with
    | None -> acc
    | Some(v) -> acc + v
    ) in
    (seq, bananas)
  )
  |> List.sort ~compare:(fun (_, i) (_, j) -> Int.compare j i)
  |> List.hd_exn
  in
  printf "got %d bananas with seq %s\n" v (ChangeSequence.to_string seq)

let () =
  let l = parse_input in
  part1 l;
  part2 l
