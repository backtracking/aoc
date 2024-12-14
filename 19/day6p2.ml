
open Format
open Lib

let orbit = H.create 16

let () = iter_lines stdin @@ fun s ->
  let x, y = split2 ~sep:')' s in
  H.add orbit y x

let inf = 1_000_000

let dist = memo (fun dist (x, y) ->
  if x = y then 0 else
  match H.find_all orbit x with
  | [] -> inf
  | [z] -> 1 + dist (z, y)
  | _ -> assert false
)

let start  = H.find orbit "YOU"
let target = H.find orbit "SAN"

let ans = H.fold (fun x _ ans -> min ans (dist (start, x) + dist (target, x)))
            orbit inf
let () = printf "%d@." ans
