let predict l =
  let buildSubSequence li =
    let toTuple l = match l with
    | hd :: _ -> (hd, List.rev l)
    | [] -> (1, [])
    in
    List.fold_left (fun (res, last) e -> ((e - last) :: res, e)) ([], List.hd li) (List.tl li) |> fst |> toTuple
  in
  let rec inner acc li =
    if (List.for_all (fun x -> x == 0) li) then acc
    else
      match buildSubSequence(li) with
      | (last, subList) -> inner (acc + last) subList
  in
  (Batteries.List.last l) + inner 0 l

let res () =
  let file = "/Users/chris/code/adventofcode_2023/data/day_nine.txt" in
  let lines = Batteries.File.lines_of(file) in
  let rec inner acc = match Batteries.Enum.get lines with
  | Some(l) -> inner (acc + predict (List.map int_of_string (String.split_on_char ' ' l)))
  | None -> acc
  in
  inner 0
