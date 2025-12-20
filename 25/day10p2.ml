
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

let maxb = ref 0
let maxj = ref 0

let parse_machine s =
  let i = String.index_from s 0 ']' in
  let t = String.sub s 1 (i-1) in
  let j = String.index_from s i '{' in
  let b = String.sub s (i+2) (j-i-3) in
  let button b = String.sub b 1 (String.length b - 2) |> split_ints ~sep:',' in
  let b = split_strings ~sep:' ' b |> List.map button in
  let k = String.index_from s j '}' in
  let l = String.sub s (j+1) (k-j-1) |> split_ints ~sep:',' in
  maxj := max !maxj (List.length l);
  maxb := max !maxb (List.length b);
  { target = t;
    buttons = b;
    joltage = l; }

let machines = map_lines stdin parse_machine
let () = printf "max %d buttons, max %d joltages@." !maxb !maxj

module M = Map.Make(Int)
module S = Set.Make(Int)
exception Yes
exception No

let solve_for ?(debug=false) fuel targets buttons =
  let nt = Array.length targets in
  let nb = Array.length buttons in
  let todo =  (* target -> joltage * set of buttons *)
    fold_int 0 nt (fun t todo -> M.add t
      (targets.(t),
       fold_int 0 nb
         (fun b s -> if List.mem t buttons.(b) then S.add b s else s) S.empty)
      todo) M.empty in
  let print todo =
    printf "@[<hov 2>todo:";
    let print_set fmt s =
      fprintf fmt "{%a}" (pp_print_list ~pp_sep:comma pp_print_int)
        (S.elements s) in
    todo |> M.iter (fun j (t, s) -> printf "@ %d(%d) => %a;" j t print_set s);
    printf "@]@.";
  in
  let press b n todo = (* press n times the button b *)
    assert (n >= 0);
    let update todo j =
      let t, s = try M.find j todo with Not_found -> raise No in
      if n > t then raise No;
      if n = t then M.remove j todo else
      let t = t - n in
      let s = S.remove b s in
      if S.is_empty s then raise No;
      M.add j (t, s) todo in
    List.fold_left update todo buttons.(b)
  in
  let rec solve fuel (todo: (int * S.t) M.t) =
    if debug then print todo;
    if fuel = 0 then raise (if M.is_empty todo then Yes else No);
    if M.is_empty todo then raise No;
    (* pick a joltage j with a minimum of buttons *)
    let j, (tj, bj) = M.fold (fun j (t,s) (_, (bt, bs) as acc) ->
      if (S.cardinal s, t) < (S.cardinal bs, bt) then (j,(t,s)) else acc)
      todo (M.choose todo) in
    let nb = S.cardinal bj in
    assert (nb > 0);
    if debug then printf "pick joltage %d, target=%d, %d buttons@." j tj nb;
    if nb = 1 then ( (* only one button => no choice *)
      let b = S.choose bj in
      if debug then printf "button %d pressed %d times (no choice)@." b tj;
      solve (fuel - tj) (press b tj todo)
    ) else ( (* choose a button *)
      let b = S.choose bj in
      let m = List.fold_left (fun m j ->
        if M.mem j todo then let t,_ = M.find j todo in min m t
        else m) tj buttons.(b) in
      for n = tj downto 0 do
        if debug then printf "button %d pressed %d times@." b n;
        try solve (fuel - n) (press b n todo) with No -> ()
      done;
      raise No
    )
  in
  solve fuel todo

let solve_for fuel targets buttons =
  try solve_for ~debug:true fuel targets buttons; assert false
  with Yes -> true | No -> false

let total = ref 0

let solve m =
  printf "%a@?" print_machine m;
  let targets = Array.of_list m.joltage in
  let buttons = Array.of_list m.buttons in
  let minf = List.fold_left max 0 m.joltage in
  let maxf = List.fold_left (+) 0 m.joltage in

  let minf = 51 in
  let maxf = 51 in

  let rec solve fuel =
    printf "solve %d =>@." fuel;
    if fuel > maxf then (printf "fuel too large!@."; exit 1);
    if solve_for fuel targets buttons then (printf "OK@."; fuel)
    else (printf "NO@."; solve (fuel + 1))
  in
  printf "  min fuel = %d@." minf;
  let n = solve minf in
  printf "  => %d@." n;
  total += n

let () = List.iter solve machines; printf "total = %d@." !total

(*
19574
*)
