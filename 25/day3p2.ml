
open Lib
open Format

let sum = ref 0

let largest s =
  let n = String.length s in
  let largest = memo (fun largest (i, d) ->
    (* largest joltage from pos i with exactly d digits *)
    if i + d > n || d = 0 then 0 else
    let x = int_of_char s.[i] in
    max (x * pow 10 (d-1) + largest (i+1, d-1))
        (largest (i+1, d))
  ) in
  sum += largest (0, 12)

let () = List.iter largest (input_lines stdin)
let () = printf "%d@." !sum
