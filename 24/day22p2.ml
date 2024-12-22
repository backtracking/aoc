
open Format
open Lib

let m = 16_777_216

let next = memo (fun next s ->
  let s = (64 * s) lxor s in
  let s = s mod m in
  let s = (s / 32) lxor s in
  let s = s mod m in
  let s = (2048 * s) lxor s in
  let s = s mod m in
  s
)
let buyers = A.of_list (map_lines stdin int_of_string)
let nbuyers = A.length buyers
let () = printf "%d buyers@." nbuyers

let first = A.map (fun _ -> H.create (1 lsl 16)) buyers
  (* sequence -> price at first occurrence, if any *)

let delta x y = (y mod 10) - (x mod 10)
let fill b n0 =
  let rec fill d01 d12 d23 n3 step = (* n3 = next^step(n0) *)
    if step <= 2000 then (
      let n4 = next n3 in
      let d34 = delta n3 n4 in
      let s = (d01, d12, d23, d34) in
      if not (H.mem first.(b) s) then H.add first.(b) s (n4 mod 10);
      fill d12 d23 d34 n4 (step+1)
    ) in
  let n1 = next n0 in
  let n2 = next n1 in
  let n3 = next n2 in
  fill (delta n0 n1) (delta n1 n2) (delta n2 n3) n3 3

let () = A.iteri fill buyers
let () = printf "tables filled@."

let ans = ref 0
let () =
  for d0 = -9 to 9 do
  for d1 = -9 to 9 do
  for d2 = -9 to 9 do
  for d3 = -9 to 9 do
    let s = (d0, d1, d2, d3) in
    let score = sum 0 nbuyers (fun b ->
                    try H.find first.(b) s with Not_found -> 0) in
    if score > !ans then ans := score
  done
  done
  done
  done;
  printf "most bananas = %d@." !ans

