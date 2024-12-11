
open Format
open Lib

let fuel x = x / 3 - 2
let sum = fold_lines stdin (fun s acc -> acc + fuel (int_of_string s)) 0
let () = printf "%d@." sum
