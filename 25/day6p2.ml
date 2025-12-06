
open Lib
open Format

module G = Grid

let g = G.read stdin
let h = G.height g and w = G.width g
let () = printf "size %dx%d@." h w

let total = ref 0

let () =
  let empty_column j = forall 0 h (fun i -> G.get g (i, j) = ' ') in
  let last = ref (-1) in
  for j = 0 to w do if j = w || empty_column j then (
    let op, acc0 = match G.get g (h - 1, !last+1) with
      | '+' -> ( + ), 0 | '*' -> ( * ), 1 | _ -> assert false in
    printf "j = %d  acc0=%d@." j acc0;
    let f c acc =
      let b = Buffer.create 16 in
      for r = 0 to h - 2 do Buffer.add_char b (G.get g (r, c)) done;
      let n = Buffer.contents b |> String.trim |> int_of_string in
      printf "  n=%d@." n;
      op acc n in
    total += fold_int (!last+1) j f acc0;
    last := j
  ) done;
  printf "total = %d@." !total
