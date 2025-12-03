
open Lib
open Format

let sum = ref 0

let largest s =
  let n = String.length s in
  let lookup d from =
    let c = Char.chr (Char.code '0' + d) in
    String.index_from_opt s from c in
  let rec scan d1 =
    match lookup d1 0 with None -> scan (d1 - 1) | Some i ->
    if i = n-1 then scan (d1 - 1) else
    for d2 = 9 downto 0 do
      match lookup d2 (i+1) with
      | None -> ()
      | Some _ -> printf "%d%d@." d1 d2; sum += (10 * d1 + d2); raise Exit
    done
  in
  try scan 9 with Exit -> ()

let () = List.iter largest (input_lines stdin)
let () = printf "%d@." !sum
