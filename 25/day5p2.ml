
open Lib
open Format

(* sorted and disjoint *)
let intervals = ref []

let add_interval itv =
  let rec insert (lo, hi as itv) = function
    | [] -> [itv]
    | (lo', _) :: _ as il when hi < lo'-1 -> itv :: il
    | (_, hi' as itv') :: il when hi'+1 < lo -> itv' :: insert itv il
    | (lo', hi') :: il -> (* overlap *) insert (min lo lo', max hi hi') il
  in
  intervals := insert itv !intervals

let read s =
  if s = "" then (
    let print (lo, hi) = printf "%d-%d " lo hi in
    List.iter print !intervals; printf "@.";
    let add n (lo, hi) = n + hi-lo+1 in
    let count = List.fold_left add 0 !intervals in
    printf "total = %d@." count;
    exit 0
  ) else match split_ints ~sep:'-' s with
    | [lo; hi] -> add_interval (lo, hi)
    | _ -> assert false

let () = iter_lines stdin read
