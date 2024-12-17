open Core

type 'a heap = {
  mutable size: int;
  mutable data: (int * 'a) array;
  init: 'a
}

let create ~size ~init =
  if size < 0 || size > Sys.max_array_length then invalid_arg "create";
  let n = max size 2 in
  { size = 0; data = Array.create ~len:n (0, init); init }

let length (h: 'a heap) = h.size

let empty (h: 'a heap) = h.size <- 0

let min_elt (h: 'a heap) =
  if (h.size <= 0) then None
  else
    let _, e = h.data.(0) in Some(e)

let expand h =
  let n = h.size in
  assert(n > 0 && n = Array.length h.data);
  let new_data = Array.create ~len:(2 * n) (0, h.init) in
  Array.blit ~src:h.data ~src_pos:0 ~dst:new_data ~dst_pos:0 ~len:n;
  h.data <- new_data

let add (h: 'a heap) (e:'a) ~score =
  let n = h.size in if n = Array.length h.data then expand h;
  let d = h.data in
  let rec inner i =
    let pi = (i - 1) / 2 in
    let v = d.(pi) in
    if (i > 0) && Int.compare (fst v) score > 0 then (
      d.(i) <- v;
      inner pi
    )
    else
      d.(i) <- (score, e)
  in
  inner n;
  h.size <- n + 1

let pop_min_elt (h: 'a heap) =
  if (h.size <= 0) then None
  else
    let n = h.size - 1
    and d = h.data
    and res = h.data.(0)
    in
    let v = h.data.(n) in
    let rec inner i =
      let j = 2 * i + 1 in
      if (j < n) then (
        let j' = j + 1 in
        let k = if j' < n && Int.compare (fst d.(j')) (fst d.(j)) < 0 then j' else j in
        if (Int.compare (fst d.(k)) (fst v) < 0) then (
          d.(i) <- d.(k);
          inner k
        )
        else
          d.(i) <- v
      )
      else
        d.(i) <- v
    in
    inner 0;
    h.size <- n;
    Some(res)

let print h =
  let d = h.data in
  if h.size > 0 then (
    printf "size=%d\n" h.size;
    for i = 0 to h.size - 2 do
      printf "%d -> " (fst d.(i))
    done;
    printf "%d\n" (fst d.(h.size - 1))
  )
