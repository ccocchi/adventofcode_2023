open Core.Std
open Core_bench.Std

(* let map3 f a =
  let l = Array.length a in
  if l = 0 then [||] else begin
    let prev = ref None in
    let current = ref (Array.unsafe_get a 0) in
    let next = ref (Some(Array.unsafe_get a 1)) in
    let r = Array.make l (f(!prev, !current, !next)) in
    for i = 2 to l - 1 do
      prev := Some(!current);
      current := Option.get (!next);
      next := if (i == l - 1) then None else Some(Array.unsafe_get a i);
      Array.unsafe_set r (i - 1) (f(!prev, !current, !next))
    done;
    r
  end

let ary = [| 1; 2; 3 ; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15|] *)

let main () =
  Random.self_init ();
  let x = Random.float 10.0 in
  let y = Random.float 10.0 in
  Command.run (Bench.make_command [
    Bench.Test.create ~name:"Float add" (fun () ->
      ignore (x +. y));
    Bench.Test.create ~name:"Float mul" (fun () ->
      ignore (x *. y));
    Bench.Test.create ~name:"Float div" (fun () ->
      ignore (x /. y));
  ])

let () = main ()
