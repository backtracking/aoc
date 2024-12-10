
open Format
open Grid
module H = Hashtbl

let g = map (fun _ -> Lib.int_of_char) (read stdin)

let nines = ref []
let () = iter (fun p c -> if c = 9 then nines := p :: !nines) g

let path = Lib.memo (fun path (p, q) ->
  p = q ||
  let x = get g p in
  fold4 (fun p' n b -> n = x+1 && path (p', q) || b) g p false
)

let add p s q = if path (p, q) then s+1 else s
let start p = List.fold_left (add p) 0 !nines
let start p c s = if c = 0 then s + start p else s

let () = printf "%d@." (fold start g 0)
