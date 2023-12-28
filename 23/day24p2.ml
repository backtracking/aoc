
open Format
open Lib

let parse s =
  let floats s =
    Scanf.sscanf s " %d, %d, %d" (fun x y z -> float x, float y, float z) in
  let pos, vel = split2 ~sep:'@' s in
  floats pos, floats vel

let lines =
  Array.of_list (List.map parse (input_lines stdin))
let n = Array.length lines
let () = printf "%d lines@." n

(* 6 variables x,y,z (start) and vx,vy,vz (velocity) to solve

   for each hailstone i, there is a time t such that

         start + t * velocity =  start(i) + t * velocity(i)

   which means the two vectors start-start(i) and velocity-velocity(i)
   are parallel and thus their cross-product is zero

   If we write down these zero cross products for 3 hailstones
   and along each axis X,Y,and Z, we get plenty of equations,
   from which we get rid of non-linearity by subtractions two by two.
*)

let eqn i j =
  let (xi,yi,zi),(vxi,vyi,vzi) = lines.(i) in
  let (xj,yj,zj),(vxj,vyj,vzj) = lines.(j) in
  [|
    [| -.(vyi -. vyj); vxi -. vxj; 0.; yi -. yj; -.(xi -. xj); 0.;
       (yi *. vxi -. yj *. vxj) -. (xi *. vyi -. xj *. vyj) |];
    [| 0.; -.(vzi -. vzj); vyi -. vyj;  0.; zi -. zj; -.(yi -. yj);
       (zi *. vyi -. zj *. vyj) -. (yi *. vzi -. yj *. vzj) |];
    [| -.(vzi -. vzj); 0.; vxi -. vxj;  zi -. zj; 0.; -.(xi -. xj);
       (zi *. vxi -. zj *. vxj) -. (xi *. vzi -. xj *. vzj)
    |];
  |]
let mat = Array.append (eqn 0 1) (eqn 0 2)
let () = printf "%a@." (Grid.print (fun fmt _ f -> fprintf fmt "%.2f" f)) mat

let x = gaussian_elimination mat
let () = printf "%a@." (Grid.print (fun fmt _ f -> fprintf fmt "%.2f " f)) x
let () = printf "sol = %.0f@." (x.(0).(6) +. x.(1).(6) +. x.(2).(6))
let () = exit 0

(*
sol = 983620716335749 WRONG ???
      983620716335750 WRONG ???
*)
