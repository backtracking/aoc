
open Format
open Lib


let diff a =
  let n = Array.length a in
  Array.init (n-1) (fun i -> a.(i+1) - a.(i))

let rec extra a =
  if Array.for_all ((=) 0) a then 0 else
  let n = Array.length a in
  let d = diff a in
  a.(n - 1) + extra d

let extrapolate s =
  let a = Array.of_list (split_ints s) in
  extra a

let sum = fold_lines stdin (fun s sum -> sum + extrapolate s) 0
let () = printf "sum = %d@." sum
