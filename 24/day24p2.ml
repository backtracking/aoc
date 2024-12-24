
open Format
open Lib

let wires = H.create 16

let read1 s =
  let w, b = split2 ~sep:':' s in
  H.add wires w (int_of_string (String.trim b))

let rules = H.create 16

let read2 s =
  Scanf.sscanf s "%s %s %s -> %s" (fun x op y z ->
    let op = match op with
      | "AND" -> (land) | "OR" -> (lor) | "XOR" -> (lxor)
      | _ -> assert false in
    H.add rules z (x, op, y))

let () =
  let first = ref true in
  let read s = if s = "" then first := false else
               if !first then read1 s else read2 s in
  iter_lines stdin read;
  printf "%d inputs, %d rules@." (H.length wires) (H.length rules)

let wire name i = sprintf "%s%02d" name i
let bit x i = (x lsr i) land 1

let set_input x y =
  H.clear wires;
  for i = 0 to 44 do
    H.replace wires (wire "x" i) (bit x i);
    H.replace wires (wire "y" i) (bit y i);
  done

(* when swapping rules, we may cause cycles, to be detected *)
exception Cycle

let compute x y =
  set_input x y;
  let rec compute w =
    try
      let b = H.find wires w in
      if b = -1 then raise Cycle;
      b
    with Not_found ->
      let x, op, y = H.find rules w in
      H.add wires w (-1);
      let b = op (compute x) (compute y) in
      H.replace wires w b;
      b
  in
  compute

let compute_bits x y =
  let c = compute x y in
  Array.init 45 (fun i -> c (wire "z" i))

let random_bits () = (Random.int (1 lsl 15) lsl 30) lor (Random.bits ())

(* random testing *)
let random_test () =
  let min = ref 45 in
  for _ = 1 to 20 do (* heuristically enough *)
    let x = random_bits () and y = random_bits () in
    let z = compute_bits x y in
    try for i = 0 to 44 do
      if bit (x+y) i <> z.(i) then (
        if i < !min then min := i;
        raise Exit
      )
    done with Exit -> ()
  done;
  !min

let () =
  let swaps = ref [] in
  let register z1 z2 =
    printf "swapping %s <-> %s@." z1 z2;
    swaps := z1 :: z2 :: !swaps;
    if List.length !swaps = 8 then (
      let l = List.sort Stdlib.compare !swaps in
      printf "%s@." (String.concat "," l);
      exit 0
    ) in
  let swap z1 z2 =
    let r1 = H.find rules z1 in
    let r2 = H.find rules z2 in
    H.replace rules z2 r1;
    H.replace rules z1 r2;
  in
  (* maintain the set of bad wires *)
  let bad_wires = H.create 16 in
  H.iter (fun w _ -> H.add bad_wires w ()) rules;
  for _ = 1 to 4 do
    let i = random_test () in
    printf "problem with bit %d@." i;
    (* then all gates involved in bits < i are fine *)
    let c = compute 0 0 in
    for j = 0 to i-1 do ignore (c (wire "z" j)) done;
    H.iter (fun w _ -> if w.[0] <> 'x' && w.[0] <> 'y' then (
                         H.remove bad_wires w;
                         printf "%s, " w)
      ) wires;
    printf "@.";
    (* repair bit i *)
    try
    H.iter (fun w1 _ -> H.iter (fun w2 _ -> if w1 < w2 then (
      swap w1 w2;
      (try
        let j = random_test () in
        if j > i then (register w1 w2; raise Exit)
      with Cycle -> ());
      swap w1 w2
    )) bad_wires) bad_wires;
    with Exit -> ();
  done

(*
sequence of faulty bits: 7, 16, 23, 27,
  swapping shj <-> z07
  swapping tpk <-> wkb
  swapping pfn <-> z23
  swapping kcd <-> z27

kcd,pfn,shj,tpk,wkb,z07,z23,z27
user	0m35.591s
*)
