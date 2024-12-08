
open Format
open Lib

let rec check f x = function
  | [] -> true
  | y :: lvl -> let d = f x y in d >= 1 && d <= 3 && check f y lvl

let is_safe = function
  | [] | [_] -> true
  | x :: y :: lvl ->
      let d = x - y in
      if d < -3 || d = 0 || d > 3 then false else
      if d > 0 then check (fun x y -> x - y) y lvl
               else check (fun x y -> y - x) y lvl

let c = ref 0
let f s = if is_safe (split_ints s) then incr c
let () = iter_lines stdin f; printf "%d@." !c



