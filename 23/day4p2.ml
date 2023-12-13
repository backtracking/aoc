
open Format
module H = Hashtbl

let tot = ref 0
let card = ref 0

let copies = H.create 16
let add_copies c n =
  H.replace copies c (n + try H.find copies c with Not_found -> 1)

let scan s acc =
  incr card;
  let nc = try H.find copies !card with Not_found -> 1 in
  match String.split_on_char ':' s with | [_; s] -> (
  match String.split_on_char '|' s with | [w; n] ->
  let win = H.create 16 in
  let add x = if x <> "" then H.replace win x () in
  List.iter add (String.split_on_char ' ' w);
  let w = ref 0 in
  let check x = if x <> "" && H.mem win x then incr w in
  List.iter check (String.split_on_char ' ' n);
  for i = 1 to !w do add_copies (!card + i) nc done;
  acc + nc
  | _ -> assert false)
  | _ -> assert false

let sum = Lib.fold_lines stdin scan 0
let () = printf "sum = %d@." sum
