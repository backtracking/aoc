
open Format
open Lib
open Machine
open Grid

let code = A.of_list (split_ints ~sep:',' (input_line stdin))

let rob = ref (0, 0)
let dir = ref N

let turn_left  = function N -> W | W -> S | S -> E | E -> N | _ -> assert false
let turn_right = function N -> E | E -> S | S -> W | W -> N | _ -> assert false

let panels = H.create 16
let black  = H.create 16

let input () =
  if H.mem black !rob then 0 else 1

let output =
  let color = ref true in
  fun v ->
    (if !color then (
       H.replace panels !rob ();
       if v = 0 then H.replace black !rob () else H.remove black !rob
     ) else (
       dir := if v = 0 then turn_left !dir else turn_right !dir;
       rob := move !dir !rob
     )
    );
    color := not !color

let () = exec code input output

let xmin, xmax, ymin, ymax =
  let update (x, y) () (xmin, xmax, ymin, ymax) =
    (min x xmin, max x xmax, min y ymin, max y ymax) in
  H.fold update black (max_int, min_int, max_int, min_int)

let () = printf "x = %d..%d@." xmin xmax
let () = printf "y = %d..%d@." ymin ymax

let g = make (xmax - xmin + 1) (ymax - ymin + 1) '#'
let () = H.iter (fun (x, y) () -> set g (x - xmin, y - ymin) '.') black
let () = printf "%a@." print_chars g

(*
x = 0..5
y = 0..42
.#..#.####.###..###..###..####..##....##..#
##..#.#....#..#.#..#.#..#.#....#..#....#...
##..#.###..#..#.#..#.#..#.###..#.......#...
.#..#.#....###..###..###..#....#.##....#..#
.#..#.#....#.#..#....#.#..#....#..#.#..#.##
#.##..####.#..#.#....#..#.#.....###..##..##
*)

