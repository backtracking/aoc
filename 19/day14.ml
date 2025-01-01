
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

let add needs (n, ch) =
  M.add ch (n + try M.find ch needs with Not_found -> 0) needs

let rec needs acc (n, ch) =
  let k, pre = H.find reactions ch in
  if is_ore pre then
    add acc (n, ch)
  else
    let f = truncate (ceil (float n /. float k)) in
    let add acc (m, ch) = needs acc (f * m, ch) in
    List.fold_left add acc pre

let todo = needs M.empty (1, "FUEL")

let () =
  let l = M.fold (fun ch n l -> (n, ch) :: l) todo [] in
  let print fmt (n, ch) = fprintf fmt "%d %s" n ch in
  printf "%a@."
    (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") print) l;
  let ore = ref 0 in
  let add (n, ch) = match H.find reactions ch with
    | k, [m, "ORE"] ->
        printf "%n %s:@." n ch;
        printf "  %d ORE => %d %s" m k ch;
        let f = truncate (ceil (float n /. float k)) in
        printf " * %d times@." f;
        ore += m * f
    | _ -> assert false in
  List.iter add l;
  printf "%d ORE for 1 FUEL@." !ore
