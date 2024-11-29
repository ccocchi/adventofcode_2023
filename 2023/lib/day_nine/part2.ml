let predict l =
  let buildSubSequence li =
    List.fold_left (fun (res, last) e -> ((e - last) :: res, e)) ([], List.hd li) (List.tl li) |> fst |> List.rev
  in
  let rec inner li =
    if (List.for_all (fun x -> x == 0) li) then 0
    else
      (List.hd li) - (inner (buildSubSequence li))
  in
  inner l

let res () =
  let file = "/Users/chris/code/adventofcode_2023/data/day_nine.txt" in
  let lines = Batteries.File.lines_of(file) in
  let rec inner acc = match Batteries.Enum.get lines with
  | Some(l) -> inner (acc + predict (List.map int_of_string (String.split_on_char ' ' l)))
  | None -> acc
  in
  inner 0
