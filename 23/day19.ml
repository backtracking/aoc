
open Format
open Lib
module H = Hashtbl

type cmp = Lt | Gt
type condition = string * cmp * int
type action = A | R | WF of string
type rule = condition * action
type workflow = rule list * action

type part = (string, int) H.t (* may be a not so good idea... *)

let workflows : (string, workflow) H.t = H.create 16

let parts =
  let condition s =
    try let x,n = split2 ~sep:'<' s in x, Lt, int_of_string n with _ ->
    try let x,n = split2 ~sep:'>' s in x, Gt, int_of_string n with _ ->
    assert false in
  let action = function | "A" -> A | "R" -> R | s -> WF s in
  let parse_rule s = let c,a = split2 ~sep:':' s in condition c, action a in
  let workflow s =
    let name,s = split2 ~sep:'{' s in
    let rec parse = function
      | [] -> assert false
      | [s] -> [], action (String.sub s 0 (String.length s - 1))
      | s :: sl -> let rl, d = parse sl in parse_rule s :: rl, d in
    H.add workflows name (parse (split_strings ~sep:',' s)) in
  let part s =
    let s = String.sub s 1 (String.length s - 2) in
    let h = H.create 4 in
    let add s = let x,n = split2 ~sep:'=' s in H.add h x (int_of_string n) in
    List.iter add (split_strings ~sep:',' s);
    h in
  let line s (first, acc) =
    if s = "" then false, acc else
    if first then (workflow s; (true, acc)) else (false, part s :: acc) in
  snd (fold_lines stdin line (true, []))

let () =
  printf "%d workflows@." (H.length workflows);
  printf "%d parts@." (List.length parts)

let condition p (x, cmp, n) =
  let v = H.find p x in
  match cmp with Lt -> v < n | Gt -> v > n

let rec exec p w =
  rules p (H.find workflows w)

and rules p = function
  | [], WF w -> exec p w
  | [], a -> a
  | (c, a) :: _, _ when condition p c ->
      (match a with WF w -> exec p w | _ -> a)
  | _ :: rl, d -> rules p (rl, d)

let parts = List.filter (fun p -> exec p "in" = A) parts
let () = printf "%d parts accepted@." (List.length parts)

let sum p = H.fold (fun _ n acc -> acc + n) p 0
let () = printf "sum = %d@." (List.fold_left (fun acc p -> acc + sum p) 0 parts)

(* part 2 *)

let all =
  let h = H.create 16 in
  List.iter (fun x -> H.add h x (1, 4000)) ["x";"m";"a";"s"];
  h
let empty =
  let h = H.create 16 in
  List.iter (fun x -> H.add h x (1, 0)) ["x";"m";"a";"s"];
  h

let condition p (x, cmp, n) =
  let min, max = H.find p x in
  if min > max then empty, empty else
  match cmp with
  | Lt -> if max < n then p, empty else
          if min >= n then empty, p else (
          let left = H.copy p and right = H.copy p in
          H.replace left  x (min, n-1);
          H.replace right x (  n, max);
          left, right
          )
  | Gt -> if min > n then p, empty else
          if max <= n then empty, p else (
          let left = H.copy p and right = H.copy p in
          H.replace right x (min, n  );
          H.replace left  x (n+1, max);
          left, right
          )

let comb p =
  assert (H.length p = 4);
  H.fold (fun _ (min,max) acc -> if min > max then 0 else acc * (max-min+1)) p 1

let rec count p w =
  rules p (H.find workflows w)

and rules p = function
  | [], WF w -> count p w
  | [], A -> comb p
  | [], R -> 0
  | (c, a) :: rl, d ->
      let yes, no = condition p c in
      rules no (rl, d) +
      match a with
      | WF w -> count yes w
      | A -> comb yes
      | R -> 0

let () = printf "count = %d@." (count all "in")
