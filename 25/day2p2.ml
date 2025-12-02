
open Lib
open Format

let ranges = split_strings ~sep:',' (input_line stdin)
let () = printf "%d ranges@." (List.length ranges)

let sum = ref 0

let seen = H.create 16
let add n = if not (H.mem seen n) then
              (H.add seen n (); printf "  => %d@." n; sum += n)

let range s =
  let slo, shi = split2 ~sep:'-' s in
  let nlo = String.length slo and nhi = String.length shi in
  let lo = int_of_string slo and hi = int_of_string shi in
  printf "%d(%d) -- %d(%d)@." lo nlo hi nhi;
  assert (nhi = nlo || nhi = nlo + 1);
  let rec repeat n s =
    let s = s ^ string_of_int n in
    if String.length s >= nlo && String.length s <= nhi then (
      let n = int_of_string s in
      if lo <= n && n <= hi then add n
    );
    if String.length s <= nhi then
      repeat n s
  in
  let n = ref 1 in
  let twice n = let s = string_of_int n in int_of_string (s ^ s) in
  while twice !n <= hi do repeat !n (string_of_int !n); incr n done

let () = List.iter range ranges
let () = printf "sum = %d@." !sum
