
open Format
open Lib
module G = Grid

let g = G.read stdin

let regions = H.create 16

let visited = G.map (fun _ _ -> false) g

let visit p r =
  if not (G.get visited p) then (
  let ar = ref 0 in
  let pe = ref 0 in
  let rec percolate p c = if c = r && not (G.get visited p) then (
    G.set visited p true;
    ar += 1;
    L.iter (fun d ->
      let p' = G.move d p in
      if not (G.inside g p') || G.get g p' <> c then pe += 1)
    [G.N; G.S; G.W; G.E];
    G.iter4 percolate g p
  ) in
  percolate p r;
  H.add regions r (!ar, !pe)
  )

let () = G.iter visit g

let () =
  H.iter (fun r (a, p) -> printf "%c: area=%d perim=%d@." r a p) regions

let sum = H.fold (fun r (a, p) s -> s + a * p) regions 0
let () = printf "%d@." sum

