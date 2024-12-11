
open Format
open Lib

let a = Array.of_list (split_ints ~sep:',' (input_line stdin))
let () = a.(1) <- 12; a.(2) <- 2

let rec exec i =
  match a.(i) with
  | 99 ->
      a.(0)
  | 1 | 2 as o ->
      let x = a.(a.(i+1)) and y = a.(a.(i+2)) and z = a.(i+3) in
      a.(z) <- if o = 1 then x+y else x*y;
      exec (i + 4)
  | _ ->
      assert false

let ans = exec 0
let () = printf "%d@." ans
