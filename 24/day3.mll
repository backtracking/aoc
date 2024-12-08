
{
  open Format
  open Lib

  let s = ref 0
}

let digit = ['0' - '9']
let num = digit digit? digit?

rule scan = parse
 | "mul(" (num as x) "," (num as y) ")"
   { s += int_of_string x * int_of_string y; scan lexbuf }
 | _
   { scan lexbuf }
 | eof
   { () }

{
  let () = scan (Lexing.from_channel stdin)
  let () = printf "%d@." !s
}
