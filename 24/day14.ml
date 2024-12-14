
open Format
open Lib

type robot = {
  mutable x: int;
  mutable y: int;
  vx: int;
  vy: int;
}

let robots = ref []
let () = iter_lines stdin @@ fun s -> Scanf.sscanf s "p=%d,%d v=%d,%d" @@
  fun x y vx vy -> robots := { x; y; vx; vy } :: !robots

let width  = int_of_string Sys.argv.(1)
let height = int_of_string Sys.argv.(2)

let move r =
  let x = r.x + r.vx in
  let y = r.y + r.vy in
  r.x <- if x < 0 then x + width  else if x >= width  then x - width  else x;
  r.y <- if y < 0 then y + height else if y >= height then y - height else y

let steps = 100
let () =
  for _ = 1 to steps do
    List.iter move !robots
  done;
  let sizex = (width - 1) / 2 and sizey = (height - 1) / 2 in
  let count x y =
    let inside r = x <= r.x && r.x < x + sizex && y <= r.y && r.y < y + sizey in
    List.fold_left (fun acc r -> if inside r then acc+1 else acc) 0 !robots in
  let ans =
    count 0 0 * count (sizex+1) 0 * count 0 (sizey+1) *
    count (sizex+1) (sizey+1) in
  printf "%d@." ans


