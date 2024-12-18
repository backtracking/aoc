
open Format
open Lib
open Grid

let size = int_of_string Sys.argv.(1)

let g = make size size '.'

let iter_succ p f =
  assert (get g p = '.');
  iter4 (fun q _ -> if get g q = '.' then f q) g p

let bfs start target =
  let pred = H.create 16 in
  let rec build_path acc v =
    if v = start then acc else
    let p = H.find pred v in build_path (p :: acc) p in
  let visited = H.create 16 in
  let module Q = Queue in
  let q = Q.create () in
  let add pr v = Q.add v q; H.add visited v (); H.add pred v pr in
  let rec loop () =
    if Q.is_empty q then raise Not_found;
    let v = Q.pop q in
    if v = target then build_path [target] target else (
    iter_succ v (fun w -> if not (H.mem visited w) then add v w);
    loop ()
    ) in
  add start start;
  loop ()


let start = (0, 0)
let target = (size - 1, size - 1)

let () =
  while true do
    let x, y = split2 ~sep:',' (input_line stdin) in
    let x = int_of_string x and y = int_of_string y in
    set g (y, x) '#';
    try ignore (bfs start target)
    with Not_found -> printf "%d,%d@." x y; printf "%a@." print_chars g; exit 0
  done


