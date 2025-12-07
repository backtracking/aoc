
open Lib
open Format
module G = Grid

let g = G.read stdin
let h = G.height g and w = G.width g
let () = printf "size %dx%d@." h w

let start = fold_int 0 w (fun j s -> if G.get g (0,j) = 'S' then j else s) 0
let () = printf "start = %d@." start

let count = memo (fun count (i,j) ->
  assert (i mod 2 = 1);
  if i = h-1 then 1 else
  match G.get g (i+1, j) with
  | '^' -> count (i+2, j-1) + count (i+2, j+1)
  | '.' -> count (i+2, j)
  | _ -> assert false
)

let () = printf "%d timelines@." (count (1, start))



