
open Format

let g = Grid.read stdin
let get = Grid.get g
let h = Grid.height g and w = Grid.width g

let c = ref 0
let xmas s p =
  try if get p = 'X' && get (s p) = 'M' && get (s (s p)) = 'A' &&
         get (s (s (s p))) = 'S' then incr c with _ -> ()
let f (i, j) _ =
  let open Grid in
  xmas east  (i, j);
  xmas west  (i, j+3);
  xmas south (i, j);
  xmas north (i+3, j);
  xmas south_east (i, j);
  xmas north_west (i+3, j+3);
  xmas north_east (i+3, j);
  xmas south_west (i, j+3)

let () = Grid.iter f g
let () = printf "%d@." !c
