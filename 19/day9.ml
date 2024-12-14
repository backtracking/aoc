
open Format
open Lib
open Machine

let code = A.of_list (split_ints ~sep:',' (input_line stdin))

let input () = int_of_string Sys.argv.(1)
let () = exec code input (printf "%d@.")
