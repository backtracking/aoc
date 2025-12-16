
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

let solve_with_z3 ?(debug=false) (eql: (string list * int) list) =
  let pp_var fmt s = fprintf fmt "%s" s in
  let vars = H.create 16 in
  let add (vl, n) =
    List.iter (fun v -> H.replace vars v ()) vl;
    if debug then
      printf "  @[sum(%a) = %d@]@." (pp_print_list ~pp_sep:comma pp_var) vl n in
  List.iter add eql;
  let file = Filename.temp_file "lp" ".smt2" in
  let c = open_out file in
  let fmt = formatter_of_out_channel c in
  H.iter (fun v () -> fprintf fmt "(declare-fun %a () Int)@\n" pp_var v) vars;
  H.iter (fun v () -> fprintf fmt "(assert (<= 0 %a))@\n" pp_var v) vars;
  List.iter (fun (vl, s) ->
      fprintf fmt "(assert (= @[(+ %a)@] %d))@\n"
             (pp_print_list ~pp_sep:pp_print_space pp_var) vl s
    ) eql;
  fprintf fmt "(check-sat)@.";
  close_out c;
  let e = Sys.command (sprintf "z3-4.14.1 %s | grep -q -w sat" file) in
  Sys.remove file;
  e = 0

let total = ref 0

let solve m =
  printf "%a@?" print_machine m;
  let targets = Array.of_list m.joltage in
  let buttons = Array.of_list m.buttons in
  let nc = Array.length targets in
  let nb = Array.length buttons in
  let eq = Array.make nc [] in
  buttons |> Array.iteri (fun b cl ->
    List.iter (fun c -> eq.(c) <- b :: eq.(c)) cl
  );
  let var i = "x" ^ string_of_int i in
  let eq = Array.mapi (fun i bl -> List.map var bl, targets.(i)) eq
           |> Array.to_list in
  let bl = fold_int 0 nb (fun b acc -> (var b) :: acc) [] in
  let rec solve fuel =
    printf "solve %d =>@." fuel;
    let eq = (bl, fuel) :: eq in
    if solve_with_z3 eq then (printf "OK@."; fuel)
    else (printf "NO@."; solve (fuel + 1))
  in
  let minf = List.fold_left max 0 m.joltage in
  printf "  min fuel = %d@." minf;
  let n = solve minf in
  printf "  => %d@." n;
  total += n

let () = List.iter solve machines; printf "total = %d@." !total

