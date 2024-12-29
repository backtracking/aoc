
open Format
open Lib

let reaction s =
  let s1, s2 = split2 ~sep:'=' s in
  let s2 = String.sub s2 2 (String.length s2 - 2) in
  let parse s = Scanf.sscanf (String.trim s) "%d %s" (fun n x -> n, x) in
  let l1 = List.map parse (split_strings ~sep:',' s1) in
  l1, parse s2

let reactions = map_lines stdin reaction

