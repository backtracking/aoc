
open Lib
open Format

let ranges = split_strings ~sep:',' (input_line stdin)
let () = printf "%d ranges@." (List.length ranges)

let sum = ref 0

let range s =
  let slo, shi = split2 ~sep:'-' s in
  let nlo = String.length slo and nhi = String.length shi in
  let lo = int_of_string slo and hi = int_of_string shi in
  printf "%d(%d) -- %d(%d)@." lo nlo hi nhi;
  assert (nhi = nlo || nhi = nlo + 1);
  let add n = printf "  => %d@." n; sum += n in
  let twice n =
    let n = let s = string_of_int n in int_of_string (s ^ s) in
    if lo <= n && n <= hi then add n
  in
  let loop min max =
    (* printf "loop %d %d@." min max; *)
    for n = min to max do twice n done in
  match nlo mod 2, nhi mod 2 with
    | _, 0 -> loop (try int_of_string (String.sub slo 0 (nlo / 2))
                    with _ -> 0)
                   (int_of_string (String.sub shi 0 (nhi / 2)))
    | 0, 1 -> loop (int_of_string (String.sub slo 0 (nlo / 2)))
                   (int_of_string (String.make (nlo / 2) '9'))
    | 1, 1 -> ()
    | _ -> assert false

let () = List.iter range ranges
let () = printf "sum = %d@." !sum
