let () =
  (* let r = Str.regexp "^\\([A-Z]+\\) = (\\([A-Z]+\\), \\([A-Z]+\\))$" in
  let s = "AAA = (BBB, CCC)" in
  match (Str.string_match r s 0) with
  | true -> print_endline (Str.matched_group 3 s)
  | false -> print_endline "false" *)

  print_int (Aoc.Day_ten.Part1.res ());
  print_newline();

  (* let list = Batteries.List.take 10 (Aoc.Day_seven.res ()) in
  let foo ((h, _): Aoc.Day_seven.hand * int) = h.str in
    List.iter
      (fun x -> print_endline (Batteries.String.of_list (foo x)))
      list *)




  (* List.iter (fun x -> print_int x; print_newline()) (Aoc.Day_five.res ()) *)
  (* List.iter (fun x -> print_int x; print_char ' ') Aoc.Day_four.res *)
  (* let str = "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1" in
  let card = Aoc.Day_four.makeCard str in
  List.iter (fun x -> print_string x; print_char ' ') (fst card);
  print_newline();
  List.iter (fun x -> print_string x; print_char ' ') (snd card);
  print_newline();
  print_string "score = ";
  print_int (Aoc.Day_four.score card) *)

  (* print_int
    (Aoc.Day_seven.compareHand
      (Aoc.Day_seven.makeHand "QQQJA")
      (Aoc.Day_seven.makeHand "KTJJT")
      ) *)


