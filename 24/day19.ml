
open Format
open Lib

let pat =
  A.of_list (List.map String.trim (split_strings ~sep:',' (input_line stdin)))
let npat = A.length pat
let () = printf "%d patterns@." npat
let () = assert (input_line stdin = "")

let possible = memo (fun possible s ->
  s = "" ||
  exists 0 npat (fun i ->
      let p = String.length pat.(i) in
      String.starts_with ~prefix:pat.(i) s &&
      possible (String.sub s p (String.length s - p)))
)

let possible s acc = if possible s then acc+1 else acc
let ans = fold_lines stdin possible 0
let () = printf "%d possible designs@." ans

