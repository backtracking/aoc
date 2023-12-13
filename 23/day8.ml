
open Format
open Lib
module H = Hashtbl

let code = input_line stdin
let len = String.length code
let () = ignore (input_line stdin)

let next = H.create 16
let add a b c = H.add next a (b, c)
let add s = Scanf.sscanf s "%3s = (%3s, %3s)" add
let () = iter_lines stdin add

let rec walk s n i =
  if s = "ZZZ" then n else
  if i = len then walk s n 0 else
  let l, r = H.find next s in
  walk (if code.[i] = 'L' then l else r) (n+1) (i+1)

let sol = walk "AAA" 0 0
let () = printf "%d@." sol
