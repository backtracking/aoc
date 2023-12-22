
open Format
open Lib
open Grid

let coord s =
  Scanf.sscanf s "%d,%d,%d" (fun x y z -> x,y,z)
let brick s =
  let a, b = split2 ~sep:'~' s in
  let a, b = coord a, coord b in
  assert (a <= b);
  a, b

let bricks = Array.of_list (List.map brick (input_lines stdin))
let () = printf "%d bricks@." (Array.length bricks)

let cmp ((_,_,z1),_) ((_,_,z2),_) = Stdlib.compare z1 z2
let () = Array.sort cmp bricks

let sizeof (xm,ym,zm) (_,(x,y,z)) =
  max xm (1+x), max ym (1+y), max zm (1+z)
let xm,ym,zm = Array.fold_left sizeof (0,0,0) bricks
let () = printf "0..%d x 0..%d x 0..%d@." xm ym zm

let empty = -1
let space =
  Array.init xm (fun _ -> Array.init ym (fun _ -> Array.make zm empty))

let falls ((x1,y1,z1), (x2,y2,_)) =
  let z = z1 - 1 in
  forall x1 (x2+1) (fun x ->
  forall y1 (y2+1) (fun y ->
    space.(x).(y).(z) = empty
  ))

let fill i ((x1,y1,z1), (x2,y2,z2)) =
  for x = x1 to x2 do for y = y1 to y2 do for z = z1 to z2 do
    space.(x).(y).(z) <- i
  done done done

let above = Array.map (fun _ -> Sint.empty) bricks
let below = Array.map (fun _ -> Sint.empty) bricks
let set_above i j = (* i is above j *)
  above.(j) <- Sint.add i above.(j);
  below.(i) <- Sint.add j below.(i)

let rec fall i ((x1,y1,z1), (x2,y2,z2) as b) =
  assert (x1 <= x2 && y1 <= y2 && z1 <= z2);
  if z1 = 1 then (* already on the ground -> fill space *)
    fill i b
  else if falls b then (
    bricks.(i) <- ((x1,y1,z1-1), (x2,y2,z2-1));
    fall i bricks.(i)
  ) else (
    fill i b;
    let z = z1 - 1 in
    for x = x1 to x2 do for y = y1 to y2 do
      let j = space.(x).(y).(z) in
      if j <> empty then set_above i j
    done done
  )

let () = Array.iteri fall bricks

let () =
  let print i s =
    printf "brick %d below" i;
    Sint.iter (fun j -> printf " %d" j) s;
    printf "@." in
  Array.iteri print above

let can_be_disintegrated i =
  let supported j = Sint.exists ((<>) i) below.(j) in
  Sint.for_all supported above.(i)

let sol = sum_array bricks (fun i _ -> iverson (can_be_disintegrated i))
let () = printf "sol = %d@." sol

(* part II *)

let rec other_would_fall falling rem =
  let would_fall i =
    not (Sint.is_empty below.(i)) &&
    Sint.subset below.(i) falling in
  let wf = Sint.filter would_fall rem in
  let n = Sint.cardinal wf in
  if n = 0 then
    0
  else
    n + other_would_fall (Sint.union falling wf) (Sint.diff rem wf)

let all_bricks = Sint.range 0 (Array.length bricks)

let sol = sum_array bricks
            (fun i _ -> other_would_fall (Sint.singleton i)
                                         (Sint.remove i all_bricks))
let () = printf "sol = %d@." sol
