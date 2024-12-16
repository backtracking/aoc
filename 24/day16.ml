
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
module W = struct
  type edge = G.E.t
  type t = int
  let weight ((p, d), (p', d')) = if p = p' then 1000 else 1
  let compare = Stdlib.compare
  let add = (+)
  let zero = 0
end
module Dij = Graph.Path.Dijkstra(G)(W)

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

let path, cost =
  let upd (_, cost as acc) d =
    let p, c = Dij.shortest_path gr (start, E) (target, d) in
    if c < cost then p, c else acc in
  List.fold_left upd ([], max_int) dirs

let () = printf "%d@." cost

let good = memo (fun good (v, c) ->
  if c = 0 then fst v = target else
  c > 0 &&
  G.fold_succ (fun w b -> b || good (w, c - W.weight (v, w))) gr v false
)
let () = assert (good ((start, E), cost))

