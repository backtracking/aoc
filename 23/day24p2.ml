
open Format
open Lib

module GE = GaussianElimination(Q)

let parse s =
  let rat s =
    Scanf.sscanf s " %d, %d, %d"
      (fun x y z -> Q.of_int x, Q.of_int y, Q.of_int z) in
  let pos, vel = split2 ~sep:'@' s in
  rat pos, rat vel

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
  Q.([|
    [| -vyi + vyj; vxi - vxj; zero; yi - yj; -xi + xj; zero;
       yi * vxi - yj * vxj - xi * vyi + xj * vyj |];
    [| zero; -vzi + vzj; vyi - vyj;  zero; zi - zj; -yi + yj;
       zi * vyi - zj * vyj - yi * vzi + yj * vzj |];
    [| -vzi + vzj; zero; vxi - vxj;  zi - zj; zero; -xi + xj;
       zi * vxi - zj * vxj - xi * vzi + xj * vzj
    |];
  |])

let select x y =
  if Q.(abs x > abs y) then x else y

let solve i j =
  let mat = Array.append (eqn 0 1) (eqn 0 2) in
  let x = GE.gaussian_elimination select mat in
  Q.(x.(0).(6) + x.(1).(6) + x.(2).(6))

let () =
  (* any pair of hailstones will give the same answer,
     but check anyway *)
  for i = 0 to n-3 do
    let f = solve i (i+2) in
    printf "%a@." Q.pp_print f
  done

(*
sol = 983620716335749 WRONG ???
      983620716335750 WRONG ???
      983620716335751 using rational numbers instead of floats; grr...
*)
