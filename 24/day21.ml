
open Format
open Lib

type dir = U | D | L | R

(*
+---+---+---+
| 7 | 8 | 9 |
+---+---+---+
| 4 | 5 | 6 |
+---+---+---+
| 1 | 2 | 3 |
+---+---+---+
    | 0 | A |
    +---+---+
*)

let numpad = function
  | '7' -> [R, '8'; D, '4']
  | '8' -> [L, '7'; R, '9'; D, '5']
  | '9' -> [D, '6'; L, '8']
  | '4' -> [U, '7'; R, '5'; D, '1']
  | '5' -> [U, '8'; L, '4'; R, '6'; D, '2']
  | '6' -> [U, '9'; D, '3'; L, '5']
  | '1' -> [U, '4'; R, '2']
  | '2' -> [L, '1'; U, '5'; R, '3'; D, '0']
  | '3' -> [U, '6'; D, 'A'; L, '2']
  | '0' -> [U, '2'; R, 'A']
  | 'A' -> [U, '3'; L, '0']
  | _ -> assert false

(*
   +---+---+
    | ^ | A |
+---+---+---+
| < | v | > |
+---+---+---+
*)

let dirpad = function
  | '^' -> [R, 'A'; D, 'v']
  | 'A' -> [L, '^'; D, '>']
  | '<' -> [R, 'v']
  | 'v' -> [L, '<'; U, '^'; R, '>']
  | '>' -> [L, 'v'; U, 'A']
  | _ -> assert false

let build_paths cl f =
  let simple s p =
    let visited = H.create 16 in
    let rec visit s = function
      | _ when H.mem visited s -> false
      | [] -> true
      | d :: p -> H.add visited s ();
                  let x = List.assoc d (f s) in visit x p
    in
    visit s p
  in
  let path = memo (fun path (s, t, len) ->
    if len = 0 then
      if s = t then [[]] else []
    else
      let add acc (d, x) =
        let xt = path (x, t, len-1) in (* FIXME: filter *)
        let xt = List.map (List.cons d) xt in
        let xt = List.filter (simple s) xt in
        xt @ acc in
      List.fold_left add [] (f s)
  ) in
  let maxlen = List.length cl - 1 in
  let h = H.create 16 in
  List.iter (fun s ->
  List.iter (fun t ->
  for len = maxlen downto 0 do
    let pl = path (s, t, len) in
    H.replace h (s, t) (pl @ try H.find h (s, t) with Not_found -> [])
  done) cl) cl;
  H.find h

let numpath =
  build_paths ['0';'1';'2';'3';'4';'5';'6';'7';'8';'9';'A'] numpad
let dirpath =
  build_paths ['<'; '^'; '>'; 'v'; 'A'] dirpad

let print_direction fmt = function
  | U -> fprintf fmt "^"
  | D -> fprintf fmt "v"
  | L -> fprintf fmt "<"
  | R -> fprintf fmt ">"
let print_path = pp_print_list ~pp_sep:(fun fmt () -> ()) print_direction

let () =
  let pl = numpath ('2', '9') in
  List.iter (fun p -> printf "%a@." print_path p) pl

let solve pad code =
  let best = ref [] in
  let rec find sol s i =
    if i = String.length code then (
      if !best = [] || L.length sol < L.length !best then best := sol
    );
    let c = code.[i] in
    let xl = pad (s, c) in
    assert false (*TODO*)
  in
  find [] 'A' 0

let () = printf "029A => %s@." (solve numpath "029A")
