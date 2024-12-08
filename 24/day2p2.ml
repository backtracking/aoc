
open Format
open Lib

let rec check ?(remove=true) f x = function
  | [] -> true
  | y :: lvl ->
      (if remove then check ~remove:false f x lvl else false) ||
      let d = f x y in d >= 1 && d <= 3 && check ~remove f y lvl

let is_safe ?(remove=true) = function
  | [] | [_] -> true
  | x :: y :: lvl ->
      let d = x - y in
      if d < -3 || d = 0 || d > 3 then false else
      if d > 0 then check ~remove (fun x y -> x - y) y lvl
               else check ~remove (fun x y -> y - x) y lvl

let is_safe = function
  | [] | [_] -> true
  | x :: y :: lvl as l ->
      is_safe ~remove:false (y :: lvl) ||
      is_safe ~remove:false (x :: lvl) ||
      is_safe l

let c = ref 0
let f s = if is_safe (split_ints s) then incr c
let () = iter_lines stdin f; printf "%d@." !c
