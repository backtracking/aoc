
open Format
open Lib
open Grid

let map = read stdin
let () = printf "%dx%d@." (height map) (width map)

type outside = bool

let portal_at = H.create 16 (* position -> portal name * outside *)
let portals = H.create 16 (* portal name * outside -> position(s) *)

let get_name ((y, x) as p) =
  let outside = x = 2 || x = width map - 3 || y = 2 || y = height map - 3 in
  let name c1 c2 = Some (String.make 1 c1 ^ String.make 1 c2, outside) in
  assert (get map p = '.');
  match get map (move N p), get map (move W p),
        get map (move S p), get map (move E p) with
  | ('A'..'Z' as c2), _, _, _ -> name (get map (move N (move N p))) c2
  | _, ('A'..'Z' as c2), _, _ -> name (get map (move W (move W p))) c2
  | _, _, ('A'..'Z' as c1), _ -> name c1 (get map (move S (move S p)))
  | _, _, _, ('A'..'Z' as c1) -> name c1 (get map (move E (move E p)))
  | _ -> None

let () =
  let add_vertex p c = match c with
    | '.' ->
        (match get_name p with
         | None -> ()
         | Some (n, o) ->
             printf "%S at (%d,%d) (%b)@." n (fst p) (snd p) o;
             H.add portal_at p (n, o);
             H.add portals (n, o) p)
    | _ -> () in
  iter add_vertex map

let start = H.find portals ("AA", true)
let target = H.find portals ("ZZ", true)

type level = int

module G = struct
  type t = unit
  module V = struct
    type t = position * level
    let equal = (=)
    let hash = Hashtbl.hash
    let compare = Stdlib.compare
  end
  module E = struct
    type t = V.t * V.t
    type label = unit
    let label _ = ()
    let src = fst
    let dst = snd
    let create _ _ _ = assert false
  end
  let iter_vertex _ _ = assert false
  let fold_vertex _ _ _ = assert false
  let iter_succ _ _ _ = assert false
  let fold_edges_e _ _ _ = assert false
  let nb_vertex _ = assert false
  let iter_succ_e f () (p, lvl as x) =
    assert (get map p = '.');
    iter4 (fun q c -> if c = '.' then f (x, (q, lvl))) map p;
    if H.mem portal_at p then
      let n, o = H.find portal_at p in
      let aazz = n = "AA" || n = "ZZ" in
      if o then (
        if lvl > 0 && not aazz then
          let q = H.find portals (n, false) in
          f (x, (q, lvl-1))
      ) else
        let q = H.find portals (n, true) in
        f (x, (q, lvl+1))

end

module W = struct
  type edge = G.E.t
  include Int
  let weight _ = 1
end
module SP = Graph.Path.Dijkstra(G)(W)

let path, n = SP.shortest_path () (start,0) (target,0)
let () = printf "%d@." n

