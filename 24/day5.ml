
open Format
open Lib

let rules = Hashtbl.create 16
let first = ref true

let sum = ref 0
let check s =
  let a = Array.of_list (split_ints ~sep:',' s) in
  let n = Array.length a in
  try
    for i = 0 to n - 2 do
      for j = 0 to n - 1 do
        if j < i && Hashtbl.mem rules (a.(i), a.(j)) then raise Exit;
        if j > i && Hashtbl.mem rules (a.(j), a.(i)) then raise Exit
      done
    done;
    sum += a.(n / 2)
  with Exit -> ()

let f s =
  if s = "" then first := false else
  if !first then
    let x,y = split2 ~sep:'|' s in
    Hashtbl.add rules (int_of_string x, int_of_string y) ()
  else
  check s
let () = iter_lines stdin f
let () = printf "%d@." !sum

