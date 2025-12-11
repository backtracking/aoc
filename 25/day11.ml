
open Lib
open Format

let g = H.create 16
let () =
  let add s =
    let src, s = split2 ~sep:':' s in
    H.add g src (split_strings s) in
  iter_lines stdin add

let count = memo (fun count v ->
  if v = "out" then 1 else
  List.fold_left (fun acc w -> acc + count w) 0 (H.find g v)
)

let () = printf "%d paths@." (count "you")


