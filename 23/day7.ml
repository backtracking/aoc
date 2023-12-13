
open Format
open Lib

type hand = { given: int array; sorted: int array; }

let value = function
  | '2'..'9' as c -> Char.code c - Char.code '0'
  | 'T' -> 10
  | 'J' -> 11
  | 'Q' -> 12
  | 'K' -> 13
  | 'A' -> 14
  | _ -> assert false

let create s =
  let given = Array.init 5 (fun i -> value s.[i]) in
  let sorted = Array.copy given in
  Array.sort Stdlib.compare sorted;
  { given; sorted }

let read s = match split_strings s with
  | [ h; bid ] -> create s, int_of_string bid
  | _ -> assert false

let kind h = match h.sorted with
  | [| a;b;c;d;e |] ->
      if a=b && a=c && a=d && a=e then
        50 (* five *)
      else if a=b && a=c && a=d || b=c && b=d && b=e then
        40 (* four *)
      else if a=b && a=c && d=e || a=b && c=d && c=e then
        30 (* full house *)
      else if a=b && b=c || b=c && c=d || c=d && d=e then
        20 (* three of a kind *)
      else if a=b && (c=d || d=e) || b=c && d=e then
        10 (* two pairs *)
      else if a=b || b=c || c=d || d=e then
        5 (* one pair *)
      else
        0 (* high card *)
  | _ -> assert false

let compare (h1,_) (h2,_) =
  let k1 = kind h1 and k2 = kind h2 in
  if k1 = k2 then Stdlib.compare h1.given h2.given
  else k1 - k2

let hands = List.map read (input_lines stdin)
let hands = Array.of_list hands
let () = printf "%d hands@." (Array.length hands)
let () = Array.sort compare hands

let sol = ref 0
let () = Array.iteri (fun i (_,bid) -> sol := !sol + (i+1)*bid) hands
let () = printf "sol = %d@." !sol

