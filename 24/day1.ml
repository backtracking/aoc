
open Lib
open Format

let l = input_lines stdin
let l = List.map (fun s -> Scanf.sscanf s "%d   %d" (fun x y -> x, y)) l
let l1, l2 = List.split l
let l1 = List.sort Stdlib.compare l1
let l2 = List.sort Stdlib.compare l2

let add s x y = s + abs (x - y)
let s = List.fold_left2 add 0 l1 l2
let () = printf "sum = %d@." s
