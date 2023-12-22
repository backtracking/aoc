
open Format
open Lib
open Grid
module S = Set.Make(struct type t = position let compare = Stdlib.compare end)

let g = read stdin
let () = printf "%dx%d@." (height g) (width g)

let start = find (fun _ c -> c = 'S') g
let () = printf "start = %d,%d@." (fst start) (snd start)
let () = set g start '.'

let nbsteps = int_of_string Sys.argv.(1)

let rec walk n s =
  if n = 0 then
    S.cardinal s
  else
    let add p c s = if c = '.' then S.add p s else s in
    let add p s = fold4 add g p s in
    walk (n-1) (S.fold add s S.empty)

let () = printf "%d plots@." (walk nbsteps (S.singleton start))
