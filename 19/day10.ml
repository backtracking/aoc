
open Format
open Lib
open Grid

let g = read stdin

let aster = H.create 16
let () = iter (fun p c -> if c = '#' then H.add aster p ()) g
let () = printf "%d asteroids@." (H.length aster)

let best = ref 0
let loc = ref (0,0)

let dist2 (x1, y1) (x2, y2) =
  let dx = x1 - x2 and dy = y1 - y2 in dx * dx + dy * dy
let sign x =
  if x < 0 then -1 else if x = 0 then 0 else 1

let test (x, y as p) () =
  let n = ref 0 in
  let sees (qx, qy as q) =
    let dq = dist2 p q in
    try H.iter (fun (rx, ry as r) () ->
            let dr = dist2 p r in
            if r <> p && dr < dq && (rx-x)*(qy-y)=(qx-x)*(ry-y) &&
               sign (rx-x) = sign (qx-x) && sign (ry-y) = sign (qy-y)
            then raise Exit
      ) aster; true
    with Exit -> false
  in
  H.iter (fun q () -> if q <> p && sees q then incr n) aster;
  if !n > !best then (printf "%d,%d => %d@." x y !n; best := !n; loc := p)

let () = H.iter test aster
let sx, sy as loc = !loc
let () = printf "station at %d,%d => %d@." sx sy !best

let two_pi = 8. *. atan 1.

let angles = H.create 16
let add (px, py as p) () = if p <> loc then (
  let dx = px - sx and dy = py - sy in
  let a = atan2 (float dy) (float (- dx)) in
  let a = if a < 0. then a +. two_pi else a in
  H.replace angles a (p :: try H.find angles a with Not_found -> [])
)
let () = H.iter add aster
let angles = H.fold (fun a pl acc -> (a, pl) :: acc) angles []
let angles = List.sort Stdlib.compare angles
let cmp p1 p2 = Stdlib.compare (dist2 loc p1) (dist2 loc p2)
let angles = List.map (fun (a, pl) -> a, List.sort cmp pl) angles

let () =
  let print fmt (x, y) = fprintf fmt "(%d,%d)" x y in
  let print (a, pl) = printf "%.2f => @[%a@]@." a (pp_print_list print) pl in
  List.iter print angles

let rec vapor1 i = function
  | [] -> i, []
  | (_, []) :: _ -> assert false
  | (a, (x, y) :: pl) :: al ->
      printf "%d: %d,%d => %d@." i x y (100 *y + x);
      if pl = [] then vapor1 (i+1) al else
      let i, al = vapor1 (i+1) al in
      i, (a, pl) :: al

let rec loop i al =
  let i, al = vapor1 i al in
  if al <> [] then loop i al

let () = loop 1 angles
