
open Lib
open Format
open Graphics

let points =
  let add s acc = match split_ints ~sep:',' s with
    | [x; y] -> (x, y) :: acc
    | _ -> assert false in
  fold_lines stdin add [] |> Array.of_list
let np = Array.length points
let () = printf "%d points@." np

let xmin, xmax, ymin, ymax =
  let f (xmin, xmax, ymin, ymax) (x, y) =
    (min xmin x, max xmax x, min ymin y, max ymax y) in
  Array.fold_left f (max_int, min_int, max_int, min_int) points
let () = printf "bb = (%d,%d) -- (%d,%d)@." xmin xmax ymin ymax

let scale = 800. /. float (max (1 + xmax - xmin) (1 + ymax - ymin))
let gx x = float (x - xmin) *. scale |> truncate
let gy y = float (y - ymin) *. scale |> truncate
let moveto (x, y) = moveto (gx x) (gy y)
let lineto (x, y) = lineto (gx x) (gy y)

let () = open_graph " 800x800"
let () =
  moveto points.(0);
  for i = 1 to np - 1 do lineto points.(i) done;
  lineto points.(0)

(* all the segments *)
let hlines = ref []
let vlines = ref []

let () =
  let line (x1, y1) (x2, y2) =
    if x1 = x2 then vlines := (x1, min y1 y2, max y1 y2) :: !vlines
               else hlines := (y1, min x1 x2, max x1 x2) :: !hlines in
  for i = 1 to np - 1 do line points.(i-1) points.(i) done;
  line points.(np - 1) points.(0)

let () = printf "%d h lines, %d v lines@."
           (List.length !hlines) (List.length !vlines)

let between x y z = x < y && y < z

(* one of the segment strictly intersects the rectangle *)
let intersected x1 y1 x2 y2 =
  let x1 = min x1 x2 and x2 = max x1 x2 in
  let y1 = min y1 y2 and y2 = max y1 y2 in
     (!hlines |> List.exists @@ fun (y, xl, xr) ->
        between y1 y y2 && xl < x2 && x1 < xr)
  || (!vlines |> List.exists @@ fun (x, yb, yt) ->
        between x1 x x2 && yb < y2 && y1 < yt)

let m = ref 0
let best = ref (0,0,0,0)

(* brute force all possible rectangles *)
let () = points |> Array.iteri @@ fun i (x1,y1) ->
  for j = i+1 to Array.length points -1 do
    let x2,y2 = points.(j) in
    if not (intersected x1 y1 x2 y2) then
      let a = (1 + abs (x1 - x2)) * (1 + abs (y1 - y2)) in
      if a > !m then (m := a; best := (x1,y1,x2,y2))
  done

let () =
  printf "max area = %d@." !m;
  let x1,y1,x2,y2 = !best in
  set_color red;
  moveto (x1,y1); lineto (x1,y2); lineto (x2,y2); lineto (x2,y1); lineto (x1,y1)

let () = ignore (read_key ())
