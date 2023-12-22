
open Format
open Lib
open Grid
module S = Set.Make(struct type t = position let compare = Stdlib.compare end)

let g = read stdin
let h = height g and w = width g
let () = printf "%dx%d@." h w

let norm (i, j) =
  (let i = i mod h in if i < 0 then i + h else i),
  (let j = j mod w in if j < 0 then j + w else j)

let start = find (fun _ c -> c = 'S') g
let () = printf "start = %d,%d@." (fst start) (snd start)
let () = set g start '.'

let nbsteps = int_of_string Sys.argv.(1)
let () = printf "%d steps@." nbsteps

let fold4 f g p acc =
  let f p acc = f p (get g (norm p)) acc in
  acc |> f (north p) |> f (west p) |> f (south p) |> f (east p)

let show s =
  let f (i,j) (mini,maxi,minj,maxj) = (min i mini, max i maxi, min j minj, max j maxj) in
  let mini, maxi, minj, maxj = S.fold f s (max_int, min_int, max_int, min_int) in
  printf "%d..%d x %d..%d@." mini maxi minj maxj;
  let g = init (maxi - mini + 1) (maxj - minj + 1)
    (fun (i,j) ->
      let i = mini + i and j = minj + j in
      let p = i,j in
      if get g (norm p) = '#' then '#' else
      if S.mem p s then 'O' else '.') in
(*
  let g = init h w (fun p ->
              if get g p = '#' then '#' else
              if S.mem p s then 'O' else '.') in
*)
  printf "%a@." print_chars g

let rec walk n s =
  if n = 0 then (
    show s;
    S.cardinal s
  ) else
    let add p c s = if c = '.' then S.add p s else s in
    let add p s = fold4 add g p s in
    walk (n-1) (S.fold add s S.empty)

let () = printf "%d plots@." (walk nbsteps (S.singleton start))
