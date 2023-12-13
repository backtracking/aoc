
open Format
open Lib
module G = Grid

let g = G.read stdin

let gal = ref []

let is_empty c = c = '.'
let empty_row i = Array.for_all is_empty g.(i)
let empty_col j = forall (fun i -> is_empty g.(i).(j)) 0 (G.height g)

let exp = 999_999 (* or 1 *)

let () =
  let row = ref 0 in
  let col = Array.make (G.width g) 0 in
  for j = 1 to G.width g - 1 do
    col.(j) <- col.(j-1) + 1;
    if empty_col j then col.(j) <- col.(j) + exp
  done;
  for i = 0 to G.height g - 1 do
    if empty_row i then row := !row + exp else
    for j = 0 to G.width g - 1 do
      if g.(i).(j) = '#' then gal := (!row, col.(j)) :: !gal
    done;
    incr row;
  done

(* let () = *)
(*   let print (i,j) = printf "%d,%d@." i j in *)
(*   List.iter print !gal *)

let gal = Array.of_list !gal

let () =
  let sum = ref 0 in
  let n = Array.length gal in
  for i = 0 to n - 1 do
    let x,y = gal.(i) in
    for j = i+1 to n - 1 do
      let x',y' = gal.(j) in
      sum := !sum + abs (x - x') + abs (y - y')
    done
  done;
  printf "sum = %d@." !sum
