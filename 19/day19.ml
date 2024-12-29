open Format
open Lib
open Grid
open Machine

let code = read stdin

let probe x y =
  let input = let first = ref true in
              fun () -> if !first then (first := false; x) else y in
  output1 code input

let () =
  let ans = ref 0 in
  for y = 0 to 49 do
    for x = 0 to 49 do
      if probe x y = 1 then incr ans
    done
  done;
  printf "%d@." !ans


