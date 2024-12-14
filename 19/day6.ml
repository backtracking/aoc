
open Format
open Lib

let orbit = H.create 16

let () = iter_lines stdin @@ fun s ->
  let x, y = split2 ~sep:')' s in
  H.add orbit y x

let count = memo (fun count x ->
  match H.find_all orbit x with
  | [] -> 0
  | [y] -> 1 + count y
  | _ -> assert false
)

let ans = H.fold (fun x _ acc -> acc + count x) orbit 0
let () = printf "%d@." ans
