
open Format
open Scanf

module Map = struct

  type range = { src: int; dst: int; len: int }
  type t = range list

  let read c =
    let s = input_line c in
    printf "%s@." s;
    let m = ref [] in
    let rec loop () = match input_line c with
      | "" | exception End_of_file -> !m
      | s -> sscanf s "%d %d %d" (fun dst src len ->
             m := { src; dst; len } :: !m; loop ()) in
    loop ()

  let rec apply m x = match m with
    | [] -> x
    | { src; dst; len } :: _ when src <= x && x < src+len -> dst + x-src
    | _ :: m -> apply m x

end

let seeds = ref []
let () =
  let s = input_line stdin in
  match String.split_on_char ':' s with | [_; s] ->
  let add x = if x <> "" then seeds := int_of_string x :: !seeds in
  List.iter add (String.split_on_char ' ' s)
  | _ -> assert false

let () = assert (input_line stdin = "")
let m1 = Map.read stdin
let m2 = Map.read stdin
let m3 = Map.read stdin
let m4 = Map.read stdin
let m5 = Map.read stdin
let m6 = Map.read stdin
let m7 = Map.read stdin
let map x = Map.apply m7 (
            Map.apply m6 (
            Map.apply m5 (
            Map.apply m4 (
            Map.apply m3 (
            Map.apply m2 (
            Map.apply m1 x))))))

let sol = ref max_int
let seed s =
  let l = map s in
  printf "seed %d => location %d@." s l;
  sol := min !sol l
let () = List.iter seed !seeds
let () = printf "minimum location = %d@." !sol

