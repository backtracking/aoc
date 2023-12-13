
open Format
open Lib
module G = Grid

let lines = Array.of_list (input_lines stdin)
let g = Array.map array_of_string lines

let loop = Array.make_matrix (Grid.height g) (Grid.width g) '.'

let rec south i j =
  loop.(i).(j) <- 'U';
  if g.(i).(j) <> 'S' then match g.(i).(j) with
  | '|' -> south (i-1) j
  | '7' -> east  i (j-1)
  | 'F' -> west  i (j+1)
  | _ -> assert false
and north i j =
  loop.(i).(j) <- 'D';
  if g.(i).(j) <> 'S' then match g.(i).(j) with
  | '|' -> north (i+1) j
  | 'L' -> west  i (j+1)
  | 'J' -> east  i (j-1)
  | _ -> assert false
and east i j =
  loop.(i).(j) <- '-';
  if g.(i).(j) <> 'S' then match g.(i).(j) with
  | '-' -> east  i (j-1)
  | 'L' -> loop.(i).(j) <- 'U'; south (i-1) j
  | 'F' -> loop.(i).(j) <- 'D'; north (i+1) j
  | _ -> assert false
and west i j =
  loop.(i).(j) <- '-';
  if g.(i).(j) <> 'S' then match g.(i).(j) with
  | '-' -> west  i (j+1)
  | 'J' -> loop.(i).(j) <- 'U'; south (i-1) j
  | '7' -> loop.(i).(j) <- 'D'; north (i+1) j
  | _ -> assert false

let start (i, j) _ = match g.(i).(j) with
  | 'S' ->
      loop.(i).(j) <- 'S';
      (if Grid.inside g (i,j+1) then match g.(i).(j+1) with
        | '-' -> loop.(i).(j+1) <- '-'; west  i     (j+2)
        | 'J' -> loop.(i).(j+1) <- 'U'; south (i-1) (j+1)
        | '7' -> loop.(i).(j+1) <- 'D'; north (i+1) (j+1)
        | _ -> ());
      (if Grid.inside g (i, j-1) then match g.(i).(j-1) with
        | '-' -> loop.(i).(j-1) <- '-'; east  i     (j-2)
        | 'F' -> loop.(i).(j-1) <- 'D'; north (i+1) (j-1)
        | 'L' -> loop.(i).(j-1) <- 'U'; south (i-1) (j-1)
        | _ -> ());
      (if Grid.inside g (i-1, j) then match g.(i-1).(j) with
        | '|' -> loop.(i-1).(j) <- 'U'; south (i-2) j
        | 'F' -> loop.(i-1).(j) <- 'U'; west  (i-1) (j+1)
        | '7' -> loop.(i-1).(j) <- 'U'; east  (i-1) (j-1)
        | _ -> ());
      (if Grid.inside g (i+1, j) then match g.(i+1).(j) with
        | '|' -> loop.(i+1).(j) <- 'D'; north (i+2) j
        | 'J' -> loop.(i+1).(j) <- 'D'; east  (i+1) (j-1)
        | 'L' -> loop.(i+1).(j) <- 'D'; west  (i+1) (j+1)
        | _ -> ());
      if G.inside g (i-1, j) && loop.(i-1).(j) = 'D' ||
         G.inside g (i+1, j) && loop.(i+1).(j) = 'D' then
        loop.(i).(j) <- 'D' else
      if G.inside g (i-1, j) && loop.(i-1).(j) = 'U' ||
         G.inside g (i+1, j) && loop.(i+1).(j) = 'U' then
        loop.(i).(j) <- 'U'
  | _ -> ()

let () = Grid.iter start g
let () = printf "%a@." (Grid.print (fun fmt _ c -> pp_print_char fmt c)) loop

let rec scan area inside last i j =
  (* printf "%d %d %b@." i j inside; *)
  if i = Grid.height loop then
    area
  else if j = Grid.width g then (
    assert (not inside);
    scan area false ' ' (i+1) 0
  ) else match loop.(i).(j) with
    | 'U' | 'D' as c ->
        scan area (if c <> last then not inside else inside) c i (j+1)
    | '-' -> scan area inside last i (j+1)
    | '.' -> scan (if inside then area+1 else area) inside ' ' i (j+1)
    | _ -> assert false

let () = printf "sol = %d@." (scan 0 false ' ' 0 0)
