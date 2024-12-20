
open Format
open Lib
open Grid

let g = read stdin
let start = find (fun _ c -> c = 'S') g
let () = set g start '.'
let target = find (fun _ c -> c = 'E') g
let () = set g target '.'

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

let path0 = bfs start target
let len0 = List.length path0
let () = printf "%d@." len0

let saves = H.create 16
let ans = ref 0

let manhattan (y1, x1) (y2, x2) = abs (y1 - y2) + abs (x1 - x2)

let a = Array.of_list path0
let () =
  for i = 0 to len0 - 1 do let pi = a.(i) in
    for j = 0 to len0 - 1 do if i <> j then let pj = a.(j) in
      let d = manhattan pi pj in
      if d <= 20 then
        let save = j - i - d in
        if save >= 0 then (
        H.replace saves save (1 + try H.find saves save with Not_found -> 0);
        if save >= 100 then incr ans;
        )
    done
  done

let () =
  let saves = H.fold (fun s n acc -> (s, n) :: acc) saves [] in
  let saves = List.sort Stdlib.compare saves in
  List.iter (fun (s, n) -> printf "%d cheats that save %d picoseconds@." n s)
    saves;
  printf "%d@." !ans


