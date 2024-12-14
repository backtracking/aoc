
open Format
open Lib
open Machine

let code = A.of_list (split_ints ~sep:',' (input_line stdin))

let phase = A.make 5 0

let ans = ref 0
let run () =
  let amp = A.init 5 (fun _ -> A.copy code) in
  let rec input i =
    let first = ref true in
    fun () -> if !first then (first := false; phase.(i)) else
              if i = 0 then 0 else output1 amp.(i-1) (input (i-1))
  in
  exec amp.(4) (input 4) (fun v ->
    for i = 0 to 4 do printf "%d," phase.(i) done; printf " => %d@." v;
    if v > !ans then ans := v)

let rec test i =
  if i = 5 then run () else
  for v = 0 to 4 do
    if forall 0 i (fun j -> phase.(j) <> v) then (phase.(i) <- v; test (i + 1))
  done

let () = test 0; printf "%d@." !ans
