
open Format
open Lib

let reaction s =
  let s1, s2 = split2 ~sep:'=' s in
  let s2 = String.sub s2 2 (String.length s2 - 2) in
  let parse s = Scanf.sscanf (String.trim s) "%d %s" (fun n x -> n, x) in
  let l1 = List.map parse (split_strings ~sep:',' s1) in
  l1, parse s2

let reactions = H.create 16 (* chemical -> int * (chemical * int) list *)

let () =
  let add s =
    let pre, (n, ch) = reaction s in
    assert (not (H.mem reactions ch));
    H.add reactions ch (n, pre) in
  iter_lines stdin add

let is_ore = function
  | [_, "ORE"] -> true
  | _ -> false

module M = Map.Make(String)
let union = M.union (fun ch n1 n2 -> Some (n1 + n2))
let print_map fmt m =
  let print fmt (ch, n) = fprintf fmt "%d %s" n ch in
  pp_print_list ~pp_sep:pp_print_space print fmt (M.to_list m)

(* lo is for leftovers : map chemical -> int

   solve n chemical ch using leftovers lo;
   returns number of required ORE and the new leftovers *)
let rec solve lo (n, ch) =
  assert (n > 0 && ch <> "ORE");
  match M.find ch lo with
  | m ->
      if m > n then 0, M.add ch (m - n) lo
      else if m = n then 0, M.remove ch lo
      else solve (M.remove ch lo) (n - m, ch)
  | exception Not_found ->
  let k, pre = H.find reactions ch in
  let f = truncate (ceil (float n /. float k)) in
  let ore, lo = match pre with
  | [m, "ORE"] ->
      f * m, lo
  | _ ->
      let add (n, lo) (p, ch) =
        let todo = f * p in
        let todo, lo = match M.find ch lo with
          | n' ->
              if n' <= todo then todo - n', M.remove ch lo
              else 0, M.add ch (n' - todo) lo
          | exception Not_found -> todo, lo in
        if todo = 0 then
          n, lo
        else
          let np, lo = solve lo (todo, ch) in (n + np, lo) in
      List.fold_left add (0, lo) pre
  in
  let lo = if f * k = n then lo else union lo (M.singleton ch (f * k - n)) in
  (* printf "solve %d %s => %d + %a@." n ch ore print_map lo; *)
  ore, lo

let n, _ = solve M.empty (1, "FUEL")
let () = printf "%d ORE for 1 FUEL@." n

(* part 2 *)

let ore = 1_000_000_000_000
let solve f = fst (solve M.empty (f, "FUEL"))

let rec binary_search fmin fmax =
  printf "%d..%d@." fmin fmax;
  (* invariant solve fmin < ore < solve fmax *)
  if fmax = fmin + 1 then fmin else
  let mid = fmin + (fmax - fmin) / 2 in
  let n = solve mid in
  if n > ore then binary_search fmin mid else binary_search mid fmax

let rec too_much f = if solve f > ore then f else too_much (2 * f)

let nf = binary_search 1 (too_much 1)
let () = printf "%d FUEL with %d ORE@." nf ore
