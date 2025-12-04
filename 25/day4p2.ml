
open Lib
open Format
module G = Grid

let g = G.read stdin
let () = printf "%dx%d@." (G.height g) (G.width g)

let sum = ref 0
let removable p = G.fold8 (fun _ c' n -> if c' = '@' then n+1 else n) g p 0 < 4

let remove p c acc =
  if c = '@' && removable p then (G.set g p '.'; incr sum; acc+1) else acc

let rec loop () =
  let n = G.fold remove g 0 in
  if n > 0 then loop ()

let () = loop ()
let () = printf "total = %d@." !sum
