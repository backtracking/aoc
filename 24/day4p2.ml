
open Format

let g = Grid.read stdin
let get = Grid.get g
let h = Grid.height g and w = Grid.width g

let c = ref 0

let word w0 w1 w2 s p =
  try get p = w0 && get (s p) = w1 && get (s (s p)) = w2
  with _ -> false
let mas = word 'M' 'A' 'S'
let sam = word 'S' 'A' 'M'
let mas_or_sam s p = mas s p || sam s p

let f (i, j) _ =
  let open Grid in
  if mas_or_sam south_east (i,j) && mas_or_sam north_east (i+2,j) then incr c

let () = Grid.iter f g
let () = printf "%d@." !c
