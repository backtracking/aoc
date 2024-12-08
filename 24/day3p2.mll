
{
  open Format
  open Lib

  let enabled = ref true
  let s = ref 0
}

let digit = ['0' - '9']
let num = digit digit? digit?

rule scan = parse
 | "mul(" (num as x) "," (num as y) ")"
   { if !enabled then s += int_of_string x * int_of_string y;
     scan lexbuf }
 | "do()"
   { enabled := true; scan lexbuf }
 | "don't()"
   { enabled := false; scan lexbuf }
 | _
   { scan lexbuf }
 | eof
   { () }

{
  let () = scan (Lexing.from_channel stdin)
  let () = printf "%d@." !s
}
