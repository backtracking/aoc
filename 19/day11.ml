
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
let white  = H.create 16

let input () =
  if H.mem white !rob then 1 else 0

let output =
  let color = ref true in
  fun v ->
    (if !color then (
       H.replace panels !rob ();
       if v = 0 then H.remove white !rob else H.replace white !rob ()
     ) else (
       dir := if v = 0 then turn_left !dir else turn_right !dir;
       rob := move !dir !rob
     )
    );
    color := not !color

let () = exec code input output
let () = printf "%d@." (H.length panels)
