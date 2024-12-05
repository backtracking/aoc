
open Lib
open Format

let l = input_lines stdin
let l = List.map (fun s -> Scanf.sscanf s "%d   %d" (fun x y -> x, y)) l
let l1, l2 = List.split l

let count x = List.fold_left (fun s y -> s + if x = y then 1 else 0) 0 l2

let add s x = s + x * count x
let s = List.fold_left add 0 l1
let () = printf "sum = %d@." s
