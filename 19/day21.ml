
open Format
open Lib
open Grid
open Machine

let code = read stdin

let send = "NOT A J\nNOT B T\nOR T J\nNOT C T\nOR T J\nAND D J\nWALK\n"
(*
  we jump iff
      (F or H or I)
  and (H or E)
  and D
  and not (A and B and C)
*)
let send = "OR F J\n\
            OR H J\n\
            OR I J \n\
            NOT H T\n\
            NOT T T \n\
            OR E T \n\
            AND T J\n\
            AND D J\n\
            NOT A T \n\
            NOT T T\n\
            AND B T\n\
            AND C T\n\
            NOT T T\n\
            AND T J\n\
            RUN\n"
let next = ref 0

let input () = let c = send.[!next] in incr next; Char.code c
let output v =
  if v > 255 then (printf "v = %d@." v; exit 0);
  output_char stdout (Char.chr v); flush stdout

let () = exec code input output
