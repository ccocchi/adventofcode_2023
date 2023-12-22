open Alcotest

let test_find_largest_movabale_element ary expected () =
  match Aoc.Day_twelve.Part1.find_largest_movable_element ary with
  | Some(i) -> check (int) "same position" expected i
  | None -> check (int) "returns none" expected (-1)

let ary = [|
  (1, 1, Aoc.Day_twelve.Part1.L);
  (2, 2, Aoc.Day_twelve.Part1.L);
  (3, 3, Aoc.Day_twelve.Part1.L);
  (4, 4, Aoc.Day_twelve.Part1.L)
|]

let impossible = [|
  (2, 2, Aoc.Day_twelve.Part1.L);
  (1, 1, Aoc.Day_twelve.Part1.L);
  (3, 3, Aoc.Day_twelve.Part1.R);
  (4, 4, Aoc.Day_twelve.Part1.R)
|]

let right_dir = [|
  ('a', 2, Aoc.Day_twelve.Part1.L);
  ('b', 3, Aoc.Day_twelve.Part1.R);
  ('c', 1, Aoc.Day_twelve.Part1.L);
|]

let fu = [|
  (1, 1, Aoc.Day_twelve.Part1.L);
  (2, 2, Aoc.Day_twelve.Part1.L);
  (3, 3, Aoc.Day_twelve.Part1.L);
  (4, 4, Aoc.Day_twelve.Part1.L);
  (4, 5, Aoc.Day_twelve.Part1.L);
  (4, 8, Aoc.Day_twelve.Part1.L);
  (4, 6, Aoc.Day_twelve.Part1.L);
  (4, 7, Aoc.Day_twelve.Part1.L);
|]


let test_bytes_matches_record b record () =
  check (bool) "matches record" true (Aoc.Day_twelve.Part1.does_match_record record b)

let suite = [
  "find largest element", [
    test_case "when element is the last one" `Quick (test_find_largest_movabale_element ary 3);
    test_case "when no elements are movable" `Quick (test_find_largest_movabale_element impossible (-1));
    test_case "when direction is right" `Quick (test_find_largest_movabale_element right_dir 1);
    test_case "fu" `Quick (test_find_largest_movabale_element fu 5)
  ];
  "matching records", [
    test_case "easy" `Quick (test_bytes_matches_record (Bytes.of_string ".#...#....###.") [1;1;3]);
    test_case "at the end" `Quick (test_bytes_matches_record (Bytes.of_string ".#.###.#.######") [1;3;1;6]);
    test_case "at the beginning" `Quick (test_bytes_matches_record (Bytes.of_string "####.#...#...") [4;1;1])
  ]
]
