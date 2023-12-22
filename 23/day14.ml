
open Format
open Lib
open Grid

let g = read stdin
let () = printf "%dx%d@." (height g) (width g)

let rec moveup i j =
  if i = 0 || g.(i-1).(j) <> '.' then i else (
    g.(i).(j) <- '.';
    g.(i-1).(j) <- 'O';
    moveup (i-1) j
  )

let column j =
  let s = ref 0 in
  for i = 0 to height g - 1 do
    if g.(i).(j) = 'O' then s += (height g - moveup i j)
  done;
  !s

let s = sum 0 (width g) column
let () = printf "sum = %d@." s
