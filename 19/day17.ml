open Format
open Lib
open Grid
open Machine

let code = read stdin

let line = ref []
let rows = ref []

let output n =
  let c = Char.chr n in
  if c = '\n' then (
    if !line <> [] then rows := A.of_list (List.rev !line) :: !rows;
    line := []
  ) else
    line := c :: !line

let () = exec code (fun () -> 0) output

let g = assert (!line = []); A.of_list (List.rev !rows)

let () = A.iteri (fun i r -> printf "%d: %d@." i (A.length r)) g

let () = printf "%a@." print_chars g

let count =
  let intersection p = fold4 (fun q c n -> if c = '#' then n+1 else n) g p 0 in
  let f (y, x) = y * x in
  fold (fun p c n -> if c = '#' && intersection p = 4 then n + f p else n) g 0
let () = printf "%d@." count
