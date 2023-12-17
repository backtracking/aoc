
open Format
open Lib
open Grid

let gr = map (fun _ c -> Char.code c - Char.code '0') (read stdin)
let h = height gr and w = width gr
let () = printf "%dx%d@." h w

(* Use Dijkstra shortest path from OCamlgraph

   A vertex is (d, n, (i, j)) where
   - d is the direction of the incoming edge
   - n is the length already traveled in that direction
   - (i,j) is the position

   We have vertices for all d in {N,S,E,W} and all n in {1,2,3},
   plus a `target` vertex.

   The cost of an edge is the cost of the position we leave.

                  (d',1,(i-1,j))  turn means length 1
                         ^
                      +--|--+
                      |  |  |
         (d,n,(i,j) ---->c----> (d,n+1,(i,j+1)  straight means length +1
                      |  |  |
                      +--|--+
                         v
                   (d'',1,(i+1,j))  turn means length 1
*)

module V = struct
  type t = direction * (*len*)int * position
  let hash = Hashtbl.hash
  let equal = (=)
  let compare = Stdlib.compare
end
module E = struct include Int let default = 0 end
module G = Graph.Imperative.Digraph.ConcreteLabeled(V)(E)

let target = (N,0,(h,w))

module W = struct
  type edge = G.E.t
  type t = int
  let compare = Stdlib.compare
  let add = (+)
  let zero = 0
  let weight e = G.E.label e
end
module D = Graph.Path.Dijkstra(G)(W)

let g = G.create ()
let () = G.add_vertex g target
let () =
  let add d n = iter (fun p _ -> G.add_vertex g (d,n,p)) gr in
  let add d = for n = 1 to 3 do add d n done in
  List.iter add [N;S;E;W]

let opposite = function N -> S | S -> N | W -> E | E -> W | _ -> assert false

let () =
  let add (d,n,(i,j as p) as v) = if v <> target then (
    let c = get gr p in
    let add d' =
      let p' = move d' p in
      if inside gr p' && d' <> opposite d && (d' <> d || n < 3) then
        G.add_edge_e g (G.E.create v c (d', (if d=d' then n+1 else 1), p')) in
    List.iter add [N;S;E;W];
    if p = (h - 1, w - 1) then
      G.add_edge_e g (G.E.create v c target)
  ) in
  G.iter_vertex add g

let () =
  (* let print_edge fmt e = *)
  (*   let _,n,(i,j) = G.E.src e and _,n',(i',j') = G.E.dst e and c = G.E.label e in *)
  (*   fprintf fmt "(%d,%d)--%d-->(%d,%d) " i j c i' j' in *)
  (* let print_path = pp_print_list print_edge in *)
  let p, hl1 = D.shortest_path g (E,1,(0,1)) target in
  (* printf "heat loss = %d@.  @[%a@]@." hl1 print_path p; *)
  let _, hl2 = D.shortest_path g (S,1,(1,0)) target in
  printf "heat loss = %d@." (min hl1 hl2);
  ()
