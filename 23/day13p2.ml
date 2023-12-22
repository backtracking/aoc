
open Format
open Lib
open Grid

let sum = ref 0

let equal g j1 j2 =
  forall 0 (height g) (fun i -> g.(i).(j1) = g.(i).(j2))

let vmirror avoid g =
  let w = width g in
  let rec check j =
    if 2 * j > w then raise Not_found;
    if j = avoid then check (j+1) else
    if forall 0 j (fun j1 -> equal g j1 (2*j-1-j1)) then j else check (j+1) in
  check 1

let vmirror avoid g =
  try vmirror avoid g with Not_found ->
  width g - vmirror (width g - avoid) (rotate_left (rotate_left g))

let hmirror avoid g =
  vmirror avoid (rotate_left g)

let solve1 g =
  try vmirror 0 g with Not_found -> 100 * hmirror 0 g

let solve2 avoid g =
  try
    vmirror avoid g
  with Not_found ->
    100 * hmirror (avoid / 100) g

let switch c =
  if c = '.' then '#' else '.'

let smudge g =
  let olds = solve1 g in
  printf "olds = %d@." olds;
  let rec iter i j =
    if i = height g then assert false;
    if j = width g then iter (i+1) 0 else (
      let old = g.(i).(j) in
      g.(i).(j) <- switch old;
      try solve2 olds g
      with Not_found -> g.(i).(j) <- old; iter i (j+1)
    ) in
  iter 0 0

let solve rows =
  let g = List.map array_of_string rows |> Array.of_list in
  sum += smudge g

let rec loop acc = match input_line stdin with
  | "" -> solve (List.rev acc); loop []
  | s -> loop (s :: acc)
  | exception End_of_file -> solve (List.rev acc)

let () = loop []
let () = printf "sum = %d@." !sum

