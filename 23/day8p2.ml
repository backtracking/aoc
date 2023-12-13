
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

let start s = s.[2] = 'A'
let stop  s = s.[2] = 'Z'

let rec walk n i s =
  if stop s then n else
  if i = len then walk n 0 s else
  let l, r = H.find next s in
  walk  (n+1) (i+1) (if code.[i] = 'L' then l else r)

let start = H.fold (fun s _ acc -> if start s then s :: acc else acc) next []
let lens = List.map (walk 0 0) start
let () = List.iter (fun l -> printf "%d@." l) lens

let rec llcm = function
  | [] -> assert false
  | [x] -> x
  | x :: y :: xl -> llcm (lcm x y :: xl)

let sol = llcm lens
let () = printf "%d@." sol
