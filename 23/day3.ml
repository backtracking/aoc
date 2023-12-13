
open Format
open Lib

let lines = input_lines stdin

let grid = Array.map array_of_string (Array.of_list lines)
let get = Grid.get grid

let height = Grid.height grid and width = Grid.width grid
let () = printf "%dx%d@." height width

let check i j =
  let b = ref false in
  Grid.all_around grid i j (fun i' j' -> match get i' j' with
                               | '0'..'9' | '.' -> ()
                               | _ -> b := true);
  !b

let rec scan sum cur sym i j =
  let sum' = if sym then sum+cur else sum in
  if i = height then sum' else
  if j = width  then scan sum' 0 false (i+1) 0 else
  match get i j with
  | '0'..'9' as c ->
      let cur = 10 * cur + Char.code c - Char.code '0' in
      let sym = sym || check i j in
      scan sum cur sym i (j+1)
  | _ ->
      scan sum' 0 false i (j+1)

let () = printf "sum = %d@." (scan 0 0 false 0 0)
