
open Format
open Lib
open Grid

let map = read stdin

let start = ref (0, 0)

module Pset =
  Set.Make(struct type t = position let compare = Stdlib.compare end)

let idx k = Char.code k - Char.code 'a'
let bit k = 1 lsl (idx k)
let emp = 0
let add k m = m lor (bit k)
let mem k m = m land (bit k) <> 0
let rmv k m = m land (lnot (bit k))

let keys =
  let s = ref emp in
  iter (fun p -> function
  | 'a'..'z' as c -> s := add c !s
  | '@' -> start := p
  | _ -> ()) map;
  set map !start '.';
  printf "%d keys@." (pop !s);
  !s

let solve = memo (fun solve (p0, toget) ->
  if toget = emp then 0 else
  let rec find_targets visited p (* : (position * char * int) list *) =
    if Pset.mem p visited then [] else
    match get map p with
    | 'A'..'Z' as d when mem (Char.lowercase_ascii d) toget -> []
    | '#' -> []
    | 'a'..'z' as k when mem k toget -> [p, k, 0]
    | 'a'..'z' | 'A'..'Z' | '.' ->
        fold4 (fun q _ acc ->
            let tl = find_targets (Pset.add p visited) q in
            List.map (fun (t, c, n) -> (t, c, n+1)) tl @ acc)
          map p []
    | _ -> assert false in
  let tl = find_targets Pset.empty p0 in
  (* List.iter (fun (_, c, n) -> printf "%c at distance %d@." c n) tl; *)
  List.fold_left
    (fun acc (p, k, n) -> min acc (n + solve (p, rmv k toget)))
    max_int tl
)

let ans = solve (!start, keys)
let () = printf "%d@." ans


