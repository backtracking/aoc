
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

let () = assert (next 123 = 15887950)
let () = assert (next (next 123) = 16495136)

let ans = ref 0
let compute s =
  let n0 = int_of_string s in
  let n2000 = repeat 2000 next n0 in
  printf "%d: %d@." n0 n2000;
  ans += n2000
let () = iter_lines stdin compute
let () = printf "sum = %d@." !ans
