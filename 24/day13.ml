
open Format
open Lib

module GE = GaussianElimination(Q)
let select x y = if Q.(abs x > abs y) then x else y
let ge = GE.gaussian_elimination select

let offset = 0
let offset = 10_000_000_000_000

let ans = ref 0

let solve () =
  let xa, ya = Scanf.scanf "Button A: X+%d, Y+%d\n" (fun x y -> x,y) in
  let xb, yb = Scanf.scanf "Button B: X+%d, Y+%d\n" (fun x y -> x,y) in
  let xt, yt = Scanf.scanf "Prize: X=%d, Y=%d\n" (fun x y -> x,y) in
  (try Scanf.scanf "\n" (()) with _ -> ());
  let xt = xt + offset and yt = yt + offset in
  let mat = Q.([| [| of_int xa; of_int xb; of_int xt |];
                  [| of_int ya; of_int yb; of_int yt |] |]) in
  let s = ge mat in
  let a = s.(0).(2) and b = s.(1).(2) in
  if Q.den a = Z.one && Q.den b = Z.one then
    let a = Q.to_int a and b = Q.to_int b in
    assert (a * xa + b * xb = xt);
    assert (a * ya + b * yb = yt);
    printf "%d %d@." a b;
    if offset > 0 || 0 <= a && a <= 100 && 0 <= b && b <= 100 then
      ans += (3*a + b)

let () =
  try while true do solve () done with End_of_file ->
  printf "%d@." !ans



