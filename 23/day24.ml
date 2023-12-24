
open Format
open Lib
open Grid

let parse s =
  let ints s =
    Scanf.sscanf s " %d, %d, %d" (fun x y z -> x,y,z) in
  let pos, vel = split2 ~sep:'@' s in
  ints pos, ints vel

let lines =
  Array.of_list (List.map parse (input_lines stdin))
let n = Array.length lines
let () = printf "%d lines@." n

(*
     x = x1 + vx1 * t = x2 + vx2 * t'
     y = y1 + vy1 * t = y2 + vy2 * t'

     vx1 * t - vx2 * t' = x2 - x1
     vy1 * t - vy2 * t' = y2 - y1
*)
let det a b
        c d = a * d - c * b
let sign x =
  if x < 0 then -1 else if x = 0 then 0 else +1

let xymin = float 200000000000000
let xymax = float 400000000000000
(* let xymin = float 7 and xymax = float 27 *)

let inside x = xymin <= x && x <= xymax

let intersect ((x1,y1,_),(vx1,vy1,_))
              ((x2,y2,_),(vx2,vy2,_)) =
  (* printf "A: %d,%d,_ @@ %d,%d,_@." x1 y1 vx1 vy1; *)
  (* printf "B: %d,%d,_ @@ %d,%d,_@." x2 y2 vx2 vy2; *)
  let a = vx1 and b = - vx2 and e = x2 - x1 in
  let c = vy1 and d = - vy2 and f = y2 - y1 in
  let d0 = det a b c d in
  if d0 = 0 then None else
  let d1 = det e b f d
  and d2 = det a e c f in
  if d0 <> 0 then (
    let t = float d1 /. float d0 in
    let t' = float d2 /. float d0 in
    (* printf "  t = %.2f  t' = %.2f@." t t'; *)
    if t >= 0. && t' >= 0. then
      Some (float x1 +. float vx1 *. t,
            float y1 +. float vy1 *. t)
    else (
      (* printf "  in the past@."; *)
      None)
  ) else (
    (* printf "  no intersection@."; *)
    None)

let () =
  let sol = ref 0 in
  for i = 0 to n - 1 do
    for j = i+1 to n - 1 do
      match intersect lines.(i) lines.(j) with
      | Some (x, y) ->
          (* printf "  intersection %.2f,%.2f@." x y; *)
          if inside x && inside y then incr sol
      | _ ->
          ()
    done
  done;
  printf "%d intersections@." !sol
