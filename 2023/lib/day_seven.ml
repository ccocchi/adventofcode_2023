let listToTuple3 = function
| [] -> failwith "listToTuple: not enough elements"
| a :: a_tail -> match a_tail with
  | [] -> failwith "listToTuple: not enough elements"
  | b :: b_tail -> match b_tail with
    | c :: [] -> (a, b, c)
    | [] -> failwith "listToTuple: not enough elements"
    | _ -> failwith "listToTuple: too many elements"

(* let cards = [| '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; 'T'; 'J'; 'Q'; 'K'; 'A'|] *)

let indexOfCard = Hashtbl.create 13;;
Hashtbl.add indexOfCard '2' 0;;
Hashtbl.add indexOfCard '3' 1;;
Hashtbl.add indexOfCard '4' 2;;
Hashtbl.add indexOfCard '5' 3;;
Hashtbl.add indexOfCard '6' 4;;
Hashtbl.add indexOfCard '7' 5;;
Hashtbl.add indexOfCard '8' 6;;
Hashtbl.add indexOfCard '9' 7;;
Hashtbl.add indexOfCard 'T' 8;;
Hashtbl.add indexOfCard 'J' (-1);; (* part2 *)
Hashtbl.add indexOfCard 'Q' 10;;
Hashtbl.add indexOfCard 'K' 11;;
Hashtbl.add indexOfCard 'A' 12;;

type hand = {
  t: int * int * int;
  str: char list;
}

let compareCard c1 c2 = Int.compare (Hashtbl.find indexOfCard c1) (Hashtbl.find indexOfCard c2)

let compareHand h1 h2 =
  let rec cmpStr s1 s2 = match (s1, s2) with
  | ([], []) -> 0
  | (a:: t1, b :: t2) -> (
    let cmp = compareCard a b in
    match cmp with
    | 0 -> cmpStr t1 t2
    | _ -> cmp
  )
  | _ -> failwith "cannot compare string with different lengths" in
  let cmpS () = cmpStr h1.str h2.str in
  match (h1.t, h2.t) with
  | ((5, _, _), (5, _, _)) -> cmpS()
  | ((5, _, _), _) -> 1
  | (_, (5, _, _)) -> -1
  | ((4, _, _), (4, _, _)) -> cmpS()
  | ((4, _, _), _) -> 1
  | (_, (4, _, _)) -> -1
  | ((3, 2, _), (3, 2, _)) -> cmpS()
  | ((3, 2, _), _) -> 1
  | (_, (3, 2, _)) -> -1
  | ((3, _, _), (3, _, _)) -> cmpS()
  | ((3, _, _), _) -> 1
  | (_, (3, _, _)) -> -1
  | ((2, 2, _), (2, 2, _)) -> cmpS()
  | ((2, 2, _), _) -> 1
  | (_, (2, 2, _)) -> -1
  | ((2, 1, _), (2, 1, _)) -> cmpS()
  | ((2, 1, _), _) -> 1
  | (_, (2, 1, _)) -> -1
  | _ -> cmpS()

let part2 n l = match l with
| hd :: tail -> hd + n :: tail
| [] -> l

let makeHand str =
  let ary = Array.make 13 0 in
  let incr c =
    let idx = Hashtbl.find indexOfCard c in
    if (idx >= 0) then
      Array.set ary idx ((Array.get ary idx) + 1)
  in
  String.iter incr str;
  {
    t = listToTuple3 (
      part2 (Batteries.String.count_char str 'J')
      (Batteries.List.take 3 (List.rev (List.sort Int.compare (Array.to_list ary)))));
    str = Batteries.String.explode str;
  }

let listToTuple2 = function
| [] -> failwith("listToTuple: error")
| a :: tail -> match tail with
  | b :: [] -> Batteries.Tuple2.make (makeHand a) (int_of_string b)
  | _ -> failwith("listToTuple: error")

let parse l =
  let sep = Str.regexp "[ \t]+" in
  listToTuple2 (Str.split sep l)

let res () =
  let file = "/Users/chris/code/adventofcode_2023/data/day_seven.txt" in
  let lines = Batteries.File.lines_of(file) |> Batteries.List.of_enum in
  let sorted = List.sort (fun a b -> compareHand (Batteries.Tuple2.first a) (Batteries.Tuple2.first b)) (List.map parse lines) in
  List.mapi (fun i (_, bet) -> (i + 1) * bet) sorted |> Batteries.List.sum
