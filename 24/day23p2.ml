
open Format
open Lib
open Graph

module G = Imperative.Graph.Concrete(String)

let g = G.create ()

let add s =
  let x, y = split2 ~sep:'-' s in
  G.add_edge g x y

let () = iter_lines stdin add
let () = printf "%d computers@." (G.nb_vertex g)

module BK = Clique.Bron_Kerbosch(G)

let cl = BK.maximalcliques g
let () = printf "%d maximal cliques@." (List.length cl)

let update (n, ans as acc) c =
  let size = List.length c in
  if size <= n then acc else
  (size, String.concat "," (List.sort Stdlib.compare c))
let n, ans = List.fold_left update (0,"") cl
let () = printf "%s (%d)@." ans n

