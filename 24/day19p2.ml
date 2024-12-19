
open Format
open Lib

let pat =
  A.of_list (List.map String.trim (split_strings ~sep:',' (input_line stdin)))
let npat = A.length pat
let () = printf "%d patterns@." npat
let () = assert (input_line stdin = "")

let count = memo (fun count s ->
  if s = "" then 1 else
  sum 0 npat (fun i ->
      let p = String.length pat.(i) in
      if String.starts_with ~prefix:pat.(i) s then
      count (String.sub s p (String.length s - p))
      else 0)
)

let count s acc = acc + count s
let ans = fold_lines stdin count 0
let () = printf "%d ways@." ans

