
open Lib
open Format
module G = Grid

let g = G.read stdin
let h = G.height g and w = G.width g
let () = printf "size %dx%d@." h w

let total = ref 0

let start = fold_int 0 w (fun j s -> if G.get g (0,j) = 'S' then j else s) 0
let () = printf "start = %d@." start
let () = G.set g (1, start) '|'

let () =
  for i = 0 to (w-2) / 2 do let i = 1 + 2 * i in
    for j = 0 to w-1 do
      if G.get g (i, j) = '|' then
        if G.get g (i+1, j) = '^' then (
          G.set g (i+1, j-1) '|'; G.set g (i+2, j-1) '|';
          G.set g (i+1, j+1) '|'; G.set g (i+2, j+1) '|';
          incr total
        ) else (
          G.set g (i+1, j) '|';
          G.set g (i+2, j) '|';
        )
    done
  done

let () = printf "%a@." G.print_chars g
let () = printf "total = %d@." !total


