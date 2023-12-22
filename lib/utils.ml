let read_whole_file filename =
  (* open_in_bin works correctly on Unix and Windows *)
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let list_to_tuple = function
  | [] -> failwith("listToTuple: error")
  | a :: tail -> match tail with
    | b :: [] -> (a, b)
    | _ -> failwith("listToTuple: error")
