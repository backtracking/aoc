
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

let rec compute w =
  try
    H.find wires w
  with Not_found ->
    let x, op, y = H.find rules w in
    let b = op (compute x) (compute y) in
    H.add wires w b;
    b

let () =
  let bits = A.make 50 0 in
  let add w =
    let i = int_of_string (String.sub w 1 2) in
    bits.(i) <- compute w in
  let add w _ = if w.[0] = 'z' then add w in
  H.iter add rules;
  let ans = ref 0 in
  for i = 49 downto 0 do
    let b = bits.(i) in
    printf "%d" bits.(i);
    ans := 2 * !ans + b
  done;
  printf " = %d@." !ans

