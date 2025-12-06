
open Lib
open Format

let intervals = ref []
let count = ref 0

let fresh s =
  let n = int_of_string s in
  let inside (lo, hi) = lo <= n && n <= hi in
  List.exists inside !intervals

let read =
  let first = ref true in fun s ->
    if s = "" then first := false else
    if !first then match split_ints ~sep:'-' s with
    | [lo; hi] -> assert (lo <= hi); intervals := (lo, hi) :: !intervals
    | _ -> assert false else
    if fresh s then incr count

let () = iter_lines stdin read
let () = printf "total = %d@." !count
