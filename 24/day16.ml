
open Format
open Lib
open Grid

let g = read stdin

let start = find (fun _ c -> c = 'S') g
let () = set g start '.'
let target = find (fun _ c -> c = 'E') g
let () = set g target '.'

let turn_left  = function N -> W | W -> S | S -> E | E -> N | _ -> assert false
let turn_right = function N -> E | E -> S | S -> W | W -> N | _ -> assert false

module G = Graph.Imperative.Digraph.Concrete(struct
  type t = position * direction
  let compare = Stdlib.compare
  let equal = (=)
  let hash = Hashtbl.hash
end)
module PQ = Heap(struct
  type t = int * G.V.t
  let compare (w1,v1) (w2,v2) =
    let cw = Stdlib.compare w2 w1 in
    if cw != 0 then cw else G.V.compare v1 v2
end)
let weight ((p, d), (p', d')) = if p = p' then 1000 else 1

let dirs = [N; E; S; W]

let gr = G.create ()
let () =
  for i = 0 to height g - 1 do
    for j = 0 to width g - 1 do
      let p = i, j in
      List.iter (fun d -> G.add_edge gr (p, d) (p, turn_left  d);
                          G.add_edge gr (p, d) (p, turn_right d);
                          let q = move d p in
                          if get g p = '.' && get g q = '.' then
                            G.add_edge gr (p, d) (q, d))
      dirs
    done
  done

(* Dijkstra's algorithm from OCamlGraph, modified to return the path
   as a predecessor map and with several predecessors when there are
   several shortest paths. *)
let shortest_path v1 v2 =
  let visited = H.create 97 in
  let dist = H.create 97 in
  let pred = H.create 97 in
  let q = PQ.create 17 in
  let rec loop () =
    if PQ.is_empty q then raise Not_found;
    let (w,v) = PQ.pop_maximum q in
    if G.V.compare v v2 = 0 then
      w, pred
    else begin
      if not (H.mem visited v) then begin
        H.add visited v ();
        G.iter_succ_e
          (fun e ->
             let ev = snd e in
             if not (H.mem visited ev) then begin
               let dev = w + weight e in
               let improvement =
                 try dev < H.find dist ev with Not_found -> true
               in
               if improvement then begin
                 H.replace dist ev dev;
                 H.replace pred ev [v];
                 PQ.add q (dev, ev)
               end else if H.mem dist ev && dev = H.find dist ev then
                 H.replace pred ev (v :: H.find pred ev)
             end)
          gr v
      end;
      loop ()
    end
  in
  PQ.add q (0, v1);
  H.add dist v1 0;
  loop ()

let cost, path =
  let upd (cost,_ as acc) d =
    let c, pred = shortest_path (start, E) (target, d) in
    if c < cost then c, pred else acc in
  List.fold_left upd (max_int, H.create 16) dirs

let () = printf "%d@." cost

let cells = H.create 16
let dfs pred t =
  let visited = H.create 16 in
  let rec dfs v = if not (H.mem visited v) then (
     H.add visited v ();
     H.replace cells (fst v) ();
     List.iter dfs (try H.find pred v with Not_found -> []))
  in
  dfs t
let () =
  let fill d =
    let c, pred = shortest_path (start, E) (target, d) in
    if c = cost then dfs pred (target, d)
  in
  List.iter fill dirs
let () = printf "%d@." (H.length cells)
