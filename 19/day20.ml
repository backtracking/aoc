
open Format
open Lib
open Grid

let map = read stdin
let () = printf "%dx%d@." (height map) (width map)

module G = Graph.Imperative.Graph.Concrete(struct
  type t = position
  let compare = Stdlib.compare
  let equal = (=)
  let hash = Hashtbl.hash
end)

let g = G.create ()

let portal_at = H.create 16 (* position -> portal name *)
let portals = H.create 16 (* portal name -> position(s) *)

let get_name p =
  let name c1 c2 = Some (String.make 1 c1 ^ String.make 1 c2) in
  assert (get map p = '.');
  match get map (move N p), get map (move W p),
        get map (move S p), get map (move E p) with
  | ('A'..'Z' as c2), _, _, _ -> name (get map (move N (move N p))) c2
  | _, ('A'..'Z' as c2), _, _ -> name (get map (move W (move W p))) c2
  | _, _, ('A'..'Z' as c1), _ -> name c1 (get map (move S (move S p)))
  | _, _, _, ('A'..'Z' as c1) -> name c1 (get map (move E (move E p)))
  | _ -> None

let print_position fmt (i, j) =
  fprintf fmt "%d,%d" i j

let add_edge p q =
  (* printf "%a -- %a@." print_position p print_position q; *)
  G.add_edge g p q

let () =
  let add_vertex p c = match c with
    | '.' ->
        G.add_vertex g p;
        (match get_name p with
         | None -> ()
         | Some n -> printf "%S at (%d,%d)@." n (fst p) (snd p);
                     H.add portal_at p n;
                     H.add portals n p)
    | _ -> () in
  iter add_vertex map;
  let add_edge p c = match c with
    | '.' ->
        iter4 (fun q c -> if c = '.' then add_edge p q) map p;
        if H.mem portal_at p then
          let n = H.find portal_at p in
          List.iter (fun q -> if q <> p then add_edge p q)
            (H.find_all portals n)
    | _ -> () in
  iter add_edge map;
  printf "%d vertices@." (G.nb_vertex g);
  printf "%d edges@." (G.nb_edges g)

let start = H.find portals "AA"
let () = printf "start = %a@." print_position start
let target = H.find portals "ZZ"
let () = printf "target = %a@." print_position target

module W = struct
  type edge = G.E.t
  include Int
  let weight _ = 1
end
module SP = Graph.Path.Dijkstra(G)(W)

let path, n = SP.shortest_path g start target
let () = printf "%d@." n
