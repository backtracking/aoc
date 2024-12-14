
open Format
open Lib

let id = int_of_string Sys.argv.(1)

let a = Array.of_list (split_ints ~sep:',' (input_line stdin))
let () = Machine.exec a (fun _ -> id) (printf "%d@.")
