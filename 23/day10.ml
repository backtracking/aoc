
open Format
open Lib

let lines = Array.of_list (input_lines stdin)
let g = Array.map array_of_string lines

let maxd = ref 0

let rec south i j d =
  maxd := max !maxd d;
  if g.(i).(j) <> 'S' then match g.(i).(j) with
  | '|' -> south (i-1) j (d+1)
  | '7' -> east  i (j-1) (d+1)
  | 'F' -> west  i (j+1) (d+1)
  | _ -> assert false
and north i j d =
  maxd := max !maxd d;
  if g.(i).(j) <> 'S' then match g.(i).(j) with
  | '|' -> north (i+1) j (d+1)
  | 'L' -> west  i (j+1) (d+1)
  | 'J' -> east  i (j-1) (d+1)
  | _ -> assert false
and east i j d =
  maxd := max !maxd d;
  if g.(i).(j) <> 'S' then match g.(i).(j) with
  | '-' -> east  i (j-1) (d+1)
  | 'L' -> south (i-1) j (d+1)
  | 'F' -> north (i+1) j (d+1)
  | _ -> assert false
and west i j d =
  maxd := max !maxd d;
  if g.(i).(j) <> 'S' then match g.(i).(j) with
  | '-' -> west i (j+1) (d+1)
  | 'J' -> south (i-1) j (d+1)
  | '7' -> north (i+1) j (d+1)
  | _ -> assert false

let start (i, j) _ = match g.(i).(j) with
  | 'S' ->
      (if Grid.inside g (i,j+1) then match g.(i).(j+1) with
        | '-' -> west  i     (j+2) 2
        | 'J' -> south (i-1) (j+1) 2
        | '7' -> north (i+1) (j+1) 2
        | _ -> ());
      (if Grid.inside g (i,j-1) then match g.(i).(j-1) with
        | '-' -> east  i     (j-2) 2
        | 'F' -> north (i+1) (j-1) 2
        | 'L' -> south (i-1) (j-1) 2
        | _ -> ());
      (if Grid.inside g (i-1,j) then match g.(i-1).(j) with
        | '|' -> south (i-2) j     2
        | 'F' -> west  (i-1) (j+1) 2
        | '7' -> east  (i-1) (j-1) 2
        | _ -> ());
      (if Grid.inside g (i+1,j) then match g.(i+1).(j) with
        | '|' -> north (i+2) j     2
        | 'J' -> east  (i+1) (j-1) 2
        | 'L' -> west  (i+1) (j+1) 2
        | _ -> ());
      printf "maxd/2 = %d@." (!maxd / 2)
  | _ -> ()

let () = Grid.iter start g
