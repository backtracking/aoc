
open Format
open Lib

let start = int_of_string Sys.argv.(1)
let finish = int_of_string Sys.argv.(2)

let correct p =
  let s = string_of_int p in
  let n = String.length s in
  exists 0 (n-1) (fun i -> s.[i]  = s.[i+1] &&
      ((i = 0   || s.[i-1] <> s.[i]) &&
      (i = n-2 || s.[i+2] <> s.[i+1]))) &&
  forall 0 (n-1) (fun i -> s.[i] <= s.[i+1])

let count = ref 0
let () = for p = start to finish do if correct p then incr count done
let () = printf "%d@." !count
