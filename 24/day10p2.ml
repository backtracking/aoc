
open Format
open Grid
module H = Hashtbl

let g = map (fun _ -> Lib.int_of_char) (read stdin)

let count = Lib.memo (fun count p ->
  let x = get g p in
  if x = 9 then 1 else
  fold4 (fun q n s -> if n = x+1 then s + count q else s) g p 0
)

let start p c s = if c = 0 then s + count p else s

let () = printf "%d@." (fold start g 0)
