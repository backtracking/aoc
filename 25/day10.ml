
open Lib
open Format

type button = int list

type machine = {
  target: string;
  buttons: button list;
  joltage: int list;
}

let print_machine fmt m =
  fprintf fmt "@[target = %s@\n" m.target;
  fprintf fmt "  buttons:";
  List.iter (fun b -> fprintf fmt " (%a)"
    (pp_print_list ~pp_sep:comma pp_print_int) b) m.buttons;
  fprintf fmt "@\n  joltage: %a"
    (pp_print_list ~pp_sep:comma pp_print_int) m.joltage;
  fprintf fmt "@]@."

let parse_machine s =
  let i = String.index_from s 0 ']' in
  let t = String.sub s 1 (i-1) in
  let j = String.index_from s i '{' in
  let b = String.sub s (i+2) (j-i-3) in
  let button b = String.sub b 1 (String.length b - 2) |> split_ints ~sep:',' in
  let b = split_strings ~sep:' ' b |> List.map button in
  let k = String.index_from s j '}' in
  let l = String.sub s (j+1) (k-j-1) |> split_ints ~sep:',' in
  { target = t;
    buttons = b;
    joltage = l; }

let machines = map_lines stdin parse_machine

let total = ref 0

let bits_of_string s =
  let b = ref 0 in
  String.iteri (fun i c -> if c = '#' then b := !b lor (1 lsl i)) s;
  !b

let solve m =
  printf "%a@?" print_machine m;
  let toggle s acc b =
    let toggle_bit s i = s lxor (1 lsl i) in
    ((), List.fold_left toggle_bit s b) :: acc in
  let module B = BFS(struct
    include Int
    type move = unit
    let success s = s=0
    let moves s = List.fold_left (toggle s) [] m.buttons
  end) in
  let _, p = B.search (bits_of_string m.target) in
  let n = List.length p in
  printf "  => %d@." n;
  total += n

let () = List.iter solve machines; printf "total = %d@." !total
