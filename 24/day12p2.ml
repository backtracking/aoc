
open Format
open Lib
open Grid

let g = read stdin

let regions = H.create 16

let visited = map (fun _ _ -> false) g

let visit p r =
  if not (get visited p) then (
  let ar = ref 0 in
  let pe = ref 0 in
  let outside q = not (inside g q) || get g q <> r in
  let inside_ q = inside g q && get g q = r in
  let corners = ref 0 in
  let corner_in d1 d2 d3 q =
    outside (move d1 q) && outside (move d2 q) && outside (move d3 q) in
  let corner1 = corner_in N W NW in
  let corner2 = corner_in N E NE in
  let corner3 = corner_in S W SW in
  let corner4 = corner_in S E SE in
  let corner_out d1 d2 d3 q =
    inside_ (move d1 q) && inside_ (move d2 q) && outside (move d3 q) in
  let corner5 = corner_out N E NE in
  let corner6 = corner_out N W NW in
  let corner7 = corner_out S E SE in
  let corner8 = corner_out S W SW in
  let corner_cross d1 d2 d3 q =
    outside (move d1 q) && outside (move d2 q) && inside_ (move d3 q) in
  let corner9  = corner_cross N E NE in
  let corner10 = corner_cross N W NW in
  let corner11 = corner_cross S E SE in
  let corner12 = corner_cross S W SW in
  let rec percolate p c = if c = r && not (get visited p) then (
    set visited p true;
    ar += 1;
    L.iter (fun d -> let p' = move d p in if outside p' then pe += 1)
      [N; S; W; E];
    List.iter (fun f -> if f p then corners += 1)
      [corner1; corner2; corner3; corner4;
       corner5; corner6; corner7; corner8;
       corner9; corner10; corner11; corner12];
   iter4 percolate g p
  ) in
  percolate p r;
  H.add regions r (!ar, !pe, !corners)
  )

let () = iter visit g

let () =
  H.iter (fun r (a, p, c) -> printf "%c: area=%d perim=%d corners=%d@." r a p c) regions

let sum = H.fold (fun r (a, _, c) s -> s + a * c) regions 0
let () = printf "%d@." sum
