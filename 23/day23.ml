
open Format
open Lib
open Grid

let g = read stdin
let h, w = height g, width g
let () = printf "%dx%d@." h w

let start = find (fun (i,j) c -> i=0 && c='.') g
let () = printf "start = %dx%d@." (fst start) (snd start)
let target = find (fun (i,j) c -> i=h-1 && c='.') g
let () = printf "target = %dx%d@." (fst target) (snd target)

let visited = map (fun _ _ -> false) g
let longest = ref 0

let rec walk len p =
  if p = target then (
    (* printf "len = %d@." len; *)
    if len > !longest then longest := len
  ) else if not (get visited p) then (
    set visited p true;
    let len = len + 1 in
    (match get g p with
(*
    | '.' | 'v' | '>' -> iter4 (move len) g p
*)
    | '.' -> iter4 (move len) g p
    | 'v' -> walk len (south p)
    | '>' -> walk len (east p)
    | _ -> assert false);
    set visited p false
  )

and move len p c =
  if c <> '#' then walk len p

let () = walk 0 start
let () = printf "max length = %d@." !longest

(* part II *)

module H = Hashtbl

let graph = H.create 16
let add org len dst =
  printf "(%d,%d) --%d--> (%d,%d)@."
    (fst org) (snd org) len (fst dst) (snd dst);
  H.add graph org (len, dst);
  H.add graph dst (len, org)

let branch_point p =
  fold4 (fun _ c acc -> acc + iverson (c <> '#')) g p 0 > 2

let rec build org len p =
  if p = target then
    add org len p
  else if get visited p then (
    if branch_point p && len > 1 then add org len p
  ) else (
    set visited p true;
    let org, len =
      if branch_point p then (add org len p; p, 0) else org, len+1 in
    let move p' c = if c <> '#' then build org len p' in
    iter4 move g p
  )

let () = build start 0 start

let visited = map (fun _ _ -> false) g
let longest = ref 0

let rec walk len p =
  if p = target then (
    if len > !longest then longest := len
  ) else if not (get visited p) then (
    set visited p true;
    let move (l,p') = walk (1+len+l) p' in
    List.iter move (H.find_all graph p);
    set visited p false
  )

let () = walk (-1) start
let () = printf "max length = %d@." !longest
