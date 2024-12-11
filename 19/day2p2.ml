
open Format
open Lib

let machine a inp1 inp2 =
  a.(1) <- inp1; a.(2) <- inp2;
  let rec exec i = match a.(i) with
  | 99 ->
      a.(0)
  | 1 | 2 as o ->
      let x = a.(a.(i+1)) and y = a.(a.(i+2)) and z = a.(i+3) in
      a.(z) <- if o = 1 then x+y else x*y;
      exec (i + 4)
  | _ ->
      assert false
  in
  exec 0

let a = Array.of_list (split_ints ~sep:',' (input_line stdin))

let () =
  for noun = 0 to 99 do for verb = 0 to 99 do
    let ans = machine (Array.copy a) noun verb in
    if ans = 19690720 then printf "%d@." (100 * noun + verb)
  done done
