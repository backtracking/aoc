
open Lib
open Format

let size = 100
let dial = ref 50
let score = ref 0

let rotate s =
  let zero = !dial = 0 in
  let n = int_of_string (String.sub s 1 (String.length s - 1)) in
  score += (n / size);
  let n = n mod size in
  (match s.[0] with
  | 'L' -> dial -= n;
           if !dial <= 0 && not zero then incr score;
  | 'R' -> dial += n;
           if !dial >= size && not zero then incr score;
  | _ -> assert false
  );
  dial := !dial mod size;
  if !dial < 0 then dial += size

let () = List.iter rotate (input_lines stdin)
let () = printf "%d@." !score
