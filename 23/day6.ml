
open Format
open Lib

(* solve x(t-x) > d *)
let solve t d =
  let t = float t and d = float d in
  let d = sqrt (t *. t -. 4. *. d) in
  let t1 = 0.5 *. (t -. d) and t2 = 0.5 *. (t +. d) in
  printf "%g..%g@." t1 t2;
  let eps = 1e-9 in (* to avoid cases where t1 or t2 is an integer *)
  1 + truncate (floor (t2 -. eps)) - truncate (ceil (t1 +. eps))

let test t d = printf "%d %d => %d@." t d (solve t d)
let () = test 7 9; test 15 40; test 30 200

let sol = solve 35 212 * solve 93 2060 * solve 73 1201 * solve 66 1044
let () = printf "sol = %d@." sol

let () = test    71530          940200
let () = test 35937366 212206012011044
