
open Format

let scan s acc =
  match String.split_on_char ':' s with | [_; s] -> (
  match String.split_on_char '|' s with | [w; n] ->
  let win = Hashtbl.create 16 in
  let add x = if x <> "" then Hashtbl.replace win x () in
  List.iter add (String.split_on_char ' ' w);
  let sc = ref 0 in
  let check x =
    if x <> "" && Hashtbl.mem win x then
      sc := (if !sc = 0 then 1 else 2 * !sc) in
  List.iter check (String.split_on_char ' ' n);
  acc + !sc
  | _ -> assert false)
  | _ -> assert false

let sum = Lib.fold_lines stdin scan 0
let () = printf "sum = %d@." sum
