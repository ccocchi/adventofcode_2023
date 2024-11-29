open Core

let _hash_algorithm s = String.fold s ~init:0 ~f:(fun acc c ->
  ((acc + (Char.to_int c)) * 17) mod 256
)

module Hashmap = struct
  type t = (string * int) list Array.t

  let create : t = Array.create ~len:256 []

  let change (hashmap: t) ~lbl ~lens ~fn =
    let hash = _hash_algorithm lbl in
    let list = hashmap.(hash) in
    let is_present = List.mem list (lbl, 0) ~equal:(fun (l1, _) (l2, _) -> String.equal l1 l2) in
    let new_list = if (not is_present) then
      match fn (lbl, lens) with | None -> list | Some(e) -> e :: list
    else
      List.fold_right list ~init:[] ~f:(fun (plbl, plens) acc ->
        let new_el =
          if String.equal lbl plbl then fn(lbl, lens)
          else Some (plbl, plens)
        in
        match new_el with | None -> acc | Some el -> el :: acc
      )
    in
    hashmap.(hash) <- new_list

  let add hashmap ~lbl ~lens = change hashmap ~lbl ~lens ~fn:(fun x -> Some x)

  let remove hashmap ~lbl = change hashmap ~lbl ~lens:0 ~fn:(fun _ -> None)

  let focusing_power hashmap =
    Array.foldi hashmap ~init:0 ~f:(fun box sum l ->
      sum + (
        l
        |> List.rev
        |> List.foldi ~init:0 ~f:(fun slot sum' (_, lens) -> sum' + ((box + 1) * (slot + 1) * lens))
      )
    )
end

let part2 input =
  let hashmap = Hashmap.create in
  let steps = String.split input ~on:',' in
  List.iter steps ~f:(fun step ->
    if String.mem step '-' then
    let lbl = String.split step ~on:'-' |> List.hd_exn in
      Hashmap.remove hashmap ~lbl
    else
      match String.split step ~on:'=' with
      | [ lbl; lens ] ->
          let lens = Int.of_string lens in
          Hashmap.add hashmap ~lbl ~lens
      | _ -> ()
  );
  Hashmap.focusing_power hashmap

let () =
  let input = In_channel.read_lines "/Users/chris/code/adventofcode_2023/data/day15.txt" |> List.hd_exn in
  let part1 = String.split input ~on:',' |> List.fold ~init:0 ~f:(fun acc s ->
    acc + _hash_algorithm s
  ) in
  printf "part1=%d\n" part1;
  printf "part2=%d\n" (part2 input);


