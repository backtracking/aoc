
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
  let corner1  q = outside (move N q) && outside (move W q) && outside (move NW q) in
  let corner2  q = outside (move N q) && outside (move E q) && outside (move NE q) in
  let corner3  q = outside (move S q) && outside (move W q) && outside (move SW q) in
  let corner4  q = outside (move S q) && outside (move E q) && outside (move SE q) in
  let corner5  q = inside_ (move N q) && inside_ (move E q) && outside (move NE q) in
  let corner6  q = inside_ (move N q) && inside_ (move W q) && outside (move NW q) in
  let corner7  q = inside_ (move S q) && inside_ (move E q) && outside (move SE q) in
  let corner8  q = inside_ (move S q) && inside_ (move W q) && outside (move SW q) in
  let corner9  q = outside (move N q) && outside (move E q) && inside_ (move NE q) in
  let corner10 q = outside (move N q) && outside (move W q) && inside_ (move NW q) in
  let corner11 q = outside (move S q) && outside (move E q) && inside_ (move SE q) in
  let corner12 q = outside (move S q) && outside (move W q) && inside_ (move SW q) in
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
