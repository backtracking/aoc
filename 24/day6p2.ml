
open Format
open Grid
module H = Hashtbl

let g = read stdin

let start = ref (0, 0)
let () = let find p c = if c = '^' then start := p in iter find g

let turn90 = function
  | N -> E
  | E -> S
  | S -> W
  | W -> N
  | _ -> assert false
let ds = function
  | N -> "N"
  | E -> "E"
  | S -> "S"
  | W -> "W"
  | _ -> assert false

let visited = H.create 16

let rec loops p d =
  assert (inside g p);
  (* printf "%d,%d %s@." (fst p) (snd p) (ds d); *)
  H.mem visited (p, d)
  ||
  (
    H.add visited (p, d) ();
    let p' = move d p in
    inside g p' && (if get g p' = '#' then loops p (turn90 d) else loops p' d)
  )

let () =
  let count = ref 0 in
  let test p c =
    if c = '.' then (
      (* printf "test (%d,%d)@." (fst p) (snd p); *)
      set g p '#';
      if loops !start N then ((* printf "%a@." print_chars g;  *)incr count);
      set g p '.';
      Hashtbl.clear visited
    ) in
  iter test g;
  printf "%d@." !count
