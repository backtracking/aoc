
open Format
open Lib

let width = 25
let height = 6

let () =
  let layer = ref 0 in
  let best = ref max_int in
  let ans = ref 0 in
  try while true do
    incr layer;
    let count = Array.make 3 0 in
    for _ = 1 to height do for _ = 1 to width do
      let c = input_char stdin in
      if c = '\n' then raise Exit;
      let c = int_of_char c in
      if c < 3 then count.(c) <- count.(c) + 1
    done done;
    let n = count.(1) * count.(2) in
    printf "layer %d => %d@." !layer n;
    if count.(0) < !best then (best := count.(0); ans := n);
  done with Exit ->
    printf "%d@." !ans


