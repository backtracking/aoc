
open Format
open Lib

module G = Graph.Imperative.Graph.Concrete(String)

let g = G.create ()
let () =
  let add s =
    let v, sl = split2 ~sep:':' s in
    List.iter (G.add_edge g v) (split_strings sl)
    in
  iter_lines stdin add;
  printf "%d vertices@." (G.nb_vertex g)

module Display = struct
  include G
  let vertex_name v = v
  let graph_attributes _ = []
  let default_vertex_attributes _ = []
  let vertex_attributes _ = []
  let default_edge_attributes _ = []
  let edge_attributes _ = []
  let get_subgraph _ = None
end
module Neato = Graph.Graphviz.Neato(Display)

let () =
  let oc = open_out "graph25.dot" in
  Neato.output_graph oc g;
  close_out oc

let () =
  G.remove_edge g "tnz" "dgt";
  G.remove_edge g "ddc" "gqm";
  G.remove_edge g "kzh" "rks"

module CC = Graph.Components.Undirected(G)
let cc = CC.components_array g

let () = printf "%d components@." (Array.length cc)
let () = printf "product of sizes = %d@."
           (Array.fold_left (fun p c -> p * List.length c) 1 cc)

