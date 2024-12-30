
open Format
open Lib

let nphases = int_of_string Sys.argv.(1)
let repeat = int_of_string Sys.argv.(2)

let s = input_line stdin
let size = String.length s
let () = printf "length %d@." size
let input = A.init size (fun i -> int_of_char s.[i])

let pat = [| 0; 1; 0; -1 |]

let total = size * repeat

let digit x = abs (x mod 10)

let p = memo (fun _ (i, j) ->
  let j = j+1 in (* shift one left *)
  pat.((j / (i+1)) mod 4)
)

(* sum i..i+len of phase ph *)
let sum = memo (fun sum (ph, i, len) ->
  assert (0 <= len && 0 <= i);
  let len = if i + len > total then max 0 (total - i) else len in
  if len = 0 then 0 else
  if ph = 0 then
    if i+len <= size then Lib.sum i (i+len) (fun j -> input.(j))
    else if i >= size then sum (ph, i-size, len)
    else sum (ph, i, size-i) + sum (ph, 0, len-(size-i))
  else if len = 1 then (
    let w = i + 1 in
    let s = ref 0 in
    let k = ref 1 in
    while !k * w - 1 < total do
      let start = !k * w - 1 in
      let m = pat.(!k mod 4) in
      if      m =  1 then s += sum (ph - 1, start, w)
      else if m = -1 then s -= sum (ph - 1, start, w);
      incr k
    done;
    digit !s
  ) else
    let len' = len / 2 in sum (ph, i, len') + sum (ph, i + len', len - len')
)

let compute ph i =
  abs (sum (ph, i, 1) mod 10)

let () =
  for ph = 0 to nphases do
    printf "phase %d: " ph;
    for i = 0 to 7 do printf "%d" (compute ph i) done; printf "@."
  done
