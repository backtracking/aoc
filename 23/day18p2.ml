
open Format
open Lib
open Grid

let dir = function
  | '0' -> E
  | '1' -> S
  | '2' -> W
  | '3' -> N
  | _ -> assert false

let moves =
  let parse s = dir s.[7], int_of_string ("0x" ^ String.sub s 2 5) in
  let parse s = Scanf.sscanf s "%c %d %s" (fun _ _ s -> parse s) in
  List.map parse (input_lines stdin)
let () = printf "%d moves@." (List.length moves)

let points = ref []
let int = ref 0 and ext = ref 0

let interior d1 d2 = match d1, d2 with
  | E, N | N, W | W, S | S, E -> true
  | N, E | W, N | S, W | E, S -> false
  | _ -> assert false

let rec walk (i,j as p) last ml =
  match ml with
  | [] -> assert (p = (0,0))
  | (d, n) :: ml ->
      points := p :: !points;
      incr (if interior last d then int else ext);
      walk (repeat n (move d) p) d ml

let () = walk (0,0) N moves

let shoelace a =
  let n = Array.length a in
  let p i = if i = -1 then a.(n-1) else if i = n then a.(0) else a.(i) in
  let x i = fst (p i) and y i = snd (p i) in
  let s = ref 0 in
  for i = 0 to n - 1 do
    s += (x i) * (y (i+1) - y (i-1))
  done;
  !s / 2

let len = List.fold_left (fun acc (_,n) -> acc+n-1) 0 moves
let () = printf "len = %d@." len
let () = printf "int = %d@." !int
let () = printf "ext = %d@." !ext

let a = Array.of_list !points
let () = printf "area = %d@." (shoelace a + len/2 + (3 * !ext + !int)/4)
