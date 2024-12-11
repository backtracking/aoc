
open Format
open Lib

let fuel x = x / 3 - 2

let req = memo (fun req x ->
  let f = fuel x in
  if f <= 0 then 0 else f + req f
)
let req x = req x

let sum = fold_lines stdin (fun s acc -> acc + req (int_of_string s)) 0
let () = printf "%d@." sum
