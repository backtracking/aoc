
open Format
open Grid
module H = Hashtbl

let g = map (fun _ -> Lib.int_of_char) (read stdin)

module S = Set.Make(struct type t = position let compare = compare end)

let count = Lib.memo (fun count p ->
  let x = get g p in
  if x = 9 then S.singleton p else
  fold4 (fun p' n s -> if n = x+1 then S.union s (count p') else s) g p S.empty
)

let start p c s = if c = 0 then s + S.cardinal (count p) else s

let () = printf "%d@." (fold start g 0)
