
open Format
open Lib
open Grid
module H = Hashtbl

let g = read stdin
let () = printf "%dx%d@." (height g) (width g)

let beam (p, d) =
  let energy = map (fun _ _ -> false) g in
  let seen = H.create 8192 in
  let rec walk (p,d as c) = if inside g p && not (H.mem seen c) then (
    H.add seen c ();
    set energy p true;
    match get g p, d with
    | '.', _
    | '-', (W | E)
    | '|', (N | S) -> walk (move d p, d)
    | '-', (N | S) -> walk (move W p, W); walk (move E p, E)
    | '|', (W | E) -> walk (move N p, N); walk (move S p, S)
    | '/', E -> walk (move N p, N)
    | '/', W -> walk (move S p, S)
    | '/', N -> walk (move E p, E)
    | '/', S -> walk (move W p, W)
    | '\\', E -> walk (move S p, S)
    | '\\', W -> walk (move N p, N)
    | '\\', N -> walk (move W p, W)
    | '\\', S -> walk (move E p, E)
    | _ -> assert false
    ) in
  walk (p, d);
  fold (fun _ b s -> s + iverson b) energy 0

let () = printf "sum = %d@." (beam ((0,0), E))

let maxe = ref 0
let () =
  let h, w = size g in
  for i = 0 to h - 1 do
    maxe := max !maxe (beam ((i, 0  ), E));
    maxe := max !maxe (beam ((i, w-1), W));
  done;
  for j = 0 to w - 1 do
    maxe := max !maxe (beam ((0  , j), S));
    maxe := max !maxe (beam ((h-1, j), N));
  done
let () = printf "max = %d@." !maxe
