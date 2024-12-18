open Core

type operand =
| Literal of int
| RegisterA
| RegisterB
| RegisterC

type instruction = Adv | Bxl | Bst | Jnz | Bxc | Out | Bdv | Cdv

let ins_of_int = function
| 0 -> Adv
| 1 -> Bxl
| 2 -> Bst
| 3 -> Jnz
| 4 -> Bxc
| 5 -> Out
| 6 -> Bdv
| 7 -> Cdv
| _ -> invalid_arg("bad instruction")

type computer = {
  mutable a: int;
  mutable b: int;
  mutable c: int;
}

let operand_of_int i ins =
  let combo = function
  | i when i >= 0 && i <= 3 -> Literal(i)
  | 4 -> RegisterA
  | 5 -> RegisterB
  | 6 -> RegisterC
  | _ -> invalid_arg("bad combo")
in
  match ins with
  | Bxl | Jnz | Bxc -> Literal(i)
  | _ -> combo i

let run instructions =
  let regs = { a = 50_000_000_000_000; b = 0; c = 0 } in
  let op_value = function
  | Literal(n) -> n
  | RegisterA -> regs.a
  | RegisterB -> regs.b
  | RegisterC -> regs.c
  in
  let rec inner res i =
    try
      let ins = ins_of_int instructions.(i) in
      let operand = op_value (operand_of_int instructions.(i + 1) ins) in
      match ins with
      | Adv -> regs.a <- regs.a / (Int.pow 2 operand); inner res (i + 2)
      | Bdv -> regs.b <- regs.a / (Int.pow 2 operand); inner res (i + 2)
      | Cdv -> regs.c <- regs.a / (Int.pow 2 operand); inner res (i + 2)
      | Bxc -> regs.b <- regs.b lxor regs.c; inner res (i + 2)
      | Bxl -> regs.b <- regs.b lxor operand; inner res (i + 2)
      | Jnz -> if (regs.a > 0) then inner res operand else inner res (i + 2)
      | Bst -> regs.b <- operand % 8; inner res (i + 2)
      | Out -> inner ((operand mod 8) :: res) (i + 2)
    with
    | Invalid_argument(_) -> List.rev res
  in
  inner [] 0

let _part2 instructions =
  let target = [2;4;1;3;7;5;4;1;1;3;0;3;5;5;3;0] in
  for i = 0 to Int.max_value - 1 do
    if List.equal Int.equal target (run instructions) then printf "%d\n" i;
  done

let () =
  (* let _ = run [|1;7|] in () *)
  (* let l = run [|0;1;5;4;3;0|] in *)
  let l = run [|2;4;1;3;7;5;4;1;1;3;0;3;5;5;3;0|] in
  print_endline "2,4,1,3,7,5,4,1,1,3,0,3,5,5,3,0";
  print_endline (List.map l ~f:string_of_int |> List.rev |> String.concat ~sep:",")
  (* part2 [|2;4;1;3;7;5;4;1;1;3;0;3;5;5;3;0|] *)


