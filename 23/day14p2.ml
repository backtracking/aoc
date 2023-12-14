
open Format
open Lib
open Grid
module H = Hashtbl

let print = print (fun fmt _ c -> pp_print_char fmt c)

let start = read stdin
let () = printf "%dx%d@." (height start) (width start)

let copy g =
  init (height g) (width g) (get g)

let rec move dir g p =
  let n = dir p in
  if inside g n && get g n = '.' then (
    set g p '.';
    set g n 'O';
    move dir g n
  )

let moveup = move north
let movedown = move south
let moveleft = move west
let moveright = move east

let north g =
  let g = copy g in
  iter (fun p c -> if c = 'O' then moveup g p) g;
  g

let west g =
  let g = copy g in
  for j = 0 to width g - 1 do for i = 0 to height g - 1 do
    if g.(i).(j) = 'O' then moveleft g (i, j)
  done done;
  g

let south g =
  let g = copy g in
  for i = height g - 1 downto 0 do for j = 0 to width g - 1 do
    if g.(i).(j) = 'O' then movedown g (i, j)
  done done;
  g

let east g =
  let g = copy g in
  for j = width g - 1 downto 0 do for i = 0 to height g - 1 do
    if g.(i).(j) = 'O' then moveright g (i, j)
  done done;
  g

let cycle g =
  north g |> west |> south |> east

let rec repeat n g =
  if n = 0 then g else repeat (n-1) (cycle g)

let h = H.create 8192
let limit = 1_000_000_000

let rec run g i =
  try
    let j = H.find h g in
    printf "cycle %d = cycle %d@." i j;
    let len = i - j in
    assert (len > 0);
    let rem = (limit - i) mod len in
    repeat rem g
  with Not_found ->
    H.add h g i;
    run (cycle g) (i+1)

let g = run start 0

let load g =
  let h = height g in
  fold (fun (i,j) c acc -> if c = 'O' then acc + h - i else acc) g 0

let () = printf "load = %d@." (load g)

