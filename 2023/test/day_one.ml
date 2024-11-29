open Alcotest

let test_find_digits str expected () =
  let result = str |> Batteries.String.to_list |> Aoc.Day_one.findDigits |> Batteries.String.of_list in
  check string "same string" result expected

let suite = [
  "two1nine", `Quick, (test_find_digits "two1nine" "219");
  "eightwothree", `Quick, (test_find_digits "eightwothree" "823");
  "29lzrxseven", `Quick, (test_find_digits "29lzrxseven" "297");
]
