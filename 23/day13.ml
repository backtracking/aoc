
open Format
open Lib
open Grid

let sum = ref 0

let equal g j1 j2 =
  forall 0 (height g) (fun i -> g.(i).(j1) = g.(i).(j2))

let vmirror g =
  let w = width g in
  let rec check j =
    if 2 * j > w then raise Not_found;
    if forall 0 j (fun j1 -> equal g j1 (2*j-1-j1)) then j else check (j+1) in
  check 1

let vmirror g =
  try vmirror g with Not_found ->
  width g - vmirror (rotate_left (rotate_left g))

let hmirror g =
  vmirror (rotate_left g)

let solve rows =
  let g = List.map array_of_string rows |> Array.of_list in
  let v = try vmirror g with Not_found -> 100 * hmirror g in
  sum += v

let rec loop acc = match input_line stdin with
  | "" -> solve (List.rev acc); loop []
  | s -> loop (s :: acc)
  | exception End_of_file -> solve (List.rev acc)

let () = loop []
let () = printf "sum = %d@." !sum

