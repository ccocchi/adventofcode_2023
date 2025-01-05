open Core

let parse_input =
  In_channel.read_lines "/Users/chris/code/adventofcode_2023/2024/data/day25.txt"
  |> List.group ~break:(fun _ l -> String.is_empty l)
  |> List.map ~f:(fun l -> List.filter l ~f:(fun s -> String.is_empty s |> not))

let build_list input ~f =
  List.filter input ~f:(fun l -> let hd = List.hd_exn l in f hd)
  |> List.map ~f:(fun l ->
    List.map l ~f:(fun s -> String.to_list s |> List.map ~f:(fun c -> if Char.equal c '#' then 1 else 0))
    |> List.reduce_exn ~f:(fun a b -> List.map2_exn a b ~f:(+))
  )

let part1 (keys: int list list) (locks: int list list) =
  List.fold ~init:0 keys ~f:(fun acc key ->
    acc + List.count locks ~f:(fun lock ->
      List.exists2_exn key lock ~f:(fun a b -> a + b > 7) |> not
    )
  )

let () =
  let input = parse_input in
  let locks = build_list input ~f:(String.equal "#####")
  and keys = build_list input ~f:(String.equal ".....") in
  printf "part1=%d\n" (part1 keys locks)


