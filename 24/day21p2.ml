
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

let char_of_dir = function
  | U -> '^'
  | D -> 'v'
  | L -> '<'
  | R -> '>'

let build_simple_paths cl f =
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
        let xt = path (x, t, len-1) in
        let xt = List.map (List.cons d) xt in
        let xt = List.filter (simple s) xt in
        xt @ acc in
      List.fold_left add [] (f s)
  ) in
  let maxlen = List.length cl - 1 in
  let h = H.create 16 in
  List.iter (fun s ->
  List.iter (fun t ->
  try for len = 0 to maxlen do
    let pl = path (s, t, len) in
    let pl = List.map (List.map char_of_dir) pl in
    if pl <> [] then (H.add h (s, t) pl; raise Exit)
  done with Exit -> ()) cl) cl;
  H.find h

let numpath =
  build_simple_paths ['0';'1';'2';'3';'4';'5';'6';'7';'8';'9';'A'] numpad
let dirpath =
  build_simple_paths ['<'; '^'; '>'; 'v'; 'A'] dirpad

let print_direction fmt d =
  fprintf fmt "%c" (char_of_dir d)
let print_path =
  pp_print_list ~pp_sep:(fun fmt () -> ()) pp_print_char
let print_sequence =
  pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "|") pp_print_string

let string_of_list = memo (fun _ l ->
  let b = Buffer.create 16 in
  List.iter (Buffer.add_char b) l;
  Buffer.add_char b 'A';
  Buffer.contents b
)

let solve pad code f =
  let rec find acc s i =
    if i = String.length code then (
      f (List.rev acc)
    ) else (
      let c = code.[i] in
      let xl = pad (s, c) in
      List.iter (fun p -> find (string_of_list p :: acc) c (i+1);
        ) xl;
    ) in
  find [] 'A' 0

(* n robots to solve sequence s *)
let robot = memo (fun robot (n, s) ->
  if n = 0 then String.length s else
  let best = ref max_int in
  solve dirpath s (fun s1 ->
    let k = List.fold_left (fun acc s1 -> acc + robot (n-1, s1)) 0 s1 in
    if k < !best then best := k
  );
  !best
)

let nborots = int_of_string Sys.argv.(1)

let complexity s =
  printf "code %s@." s;
  let best = ref max_int in
  solve numpath s (fun s1 ->
    printf "s1 = %a@." print_sequence s1;
    let f acc s = acc + robot (nborots, s) in
    let n = List.fold_left f 0 s1 in
    if n < !best then best := n
  );
  printf "  => %d@." !best;
  !best * int_of_string (String.sub s 0 (String.length s - 1))

let ans = fold_lines stdin (fun s acc -> acc + complexity s) 0
let () = printf "%d@." ans

(*
306335137543664
user	0m0.008s
*)


