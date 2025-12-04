
open Lib
open Format
module G = Grid

let g = G.read stdin
let () = printf "%dx%d@." (G.height g) (G.width g)

let sum = ref 0
let removable p = G.fold8 (fun _ c' n -> if c' = '@' then n+1 else n) g p 0 < 4
let count p c = if c = '@' && removable p then incr sum
let () = G.iter count g
let () = printf "total = %d@." !sum
