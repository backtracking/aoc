
open Format
open Lib

let rec hash s acc i =
  if i = String.length s then acc else
  hash s (((acc + Char.code s.[i]) * 17) mod 256) (i+1)
let hash s = hash s 0 0

let () = assert (hash "cm-" = 253)

let s = input_line stdin
let l = split_strings ~sep:',' s
let sum = List.fold_left (fun acc s -> acc + hash s) 0 l
let () = printf "sum = %d@." sum
