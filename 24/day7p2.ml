
open Format
open Lib

let sum = ref 0

let (++) x y = int_of_string (string_of_int x ^ string_of_int y)

let rec solvable t = function
  | [] -> assert false
  | [x] -> x = t
  | x :: y :: xl -> solvable t ((x + y) :: xl) ||
                    solvable t ((x * y) :: xl) ||
                    solvable t ((x++ y) :: xl)

let f s =
  let t, xl = split2 ~sep:':' s in
  let t = int_of_string t and xl = split_ints xl in
  if solvable t xl then sum += t

let () = iter_lines stdin f
let () = printf "%d@." !sum

