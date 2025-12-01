
open Lib
open Format

let size = 100
let dial = ref 50
let score = ref 0

let rotate s =
  let n = int_of_string (String.sub s 1 (String.length s - 1)) in
  (match s.[0] with
  | 'L' -> dial -= n
  | 'R' -> dial += n
  | _ -> assert false);
  dial := !dial mod size;
  if !dial < 0 then dial += size;
  if !dial = 0 then incr score

let () = List.iter rotate (input_lines stdin)
let () = printf "%d@." !score
