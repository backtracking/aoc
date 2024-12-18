
open Format
open Lib
open Grid

let size = int_of_string Sys.argv.(1)
let steps = int_of_string Sys.argv.(2)

let g = make size size '.'

let () =
  for _ = 1 to steps do
    let x, y = split2 ~sep:',' (input_line stdin) in
    set g (int_of_string y, int_of_string x) '#'
  done;
  printf "%a@." print_chars g

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
    let v = Q.pop q in
    if v = target then build_path [target] target else (
    iter_succ v (fun w -> if not (H.mem visited w) then add v w);
    loop ()
    ) in
  add start start;
  loop ()


let start = (0, 0)
let target = (size - 1, size - 1)
let ans = bfs start target
let () = List.iter (fun p -> set g p 'O') ans
let () = printf "%a@." print_chars g
let () = printf "%d@." (List.length ans - 1)


