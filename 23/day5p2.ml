
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

  (* t -> itv -> itv list *)
  let rec apply_range m (x,w as i) = match m with
    | [] -> [i]
    | { src; dst; len } :: m ->
        (if x < src then
           apply_range m (x, min w (src-x))
         else []) @
        (if x+w > src && x < src+len then
           let x, w = max x src, min w (min len (src+len-x)) in
           [dst+x-src, w]
         else []) @
        (if x+w > src+len then
           let x, w = max x (src+len), min w (x+w-(src+len)) in
           apply_range m (x, w)
         else [])

  let apply_ranges m il =
    List.fold_left (fun acc i -> apply_range m i @ acc) [] il

end

let seeds = ref []
let () =
  let s = input_line stdin in
  match String.split_on_char ':' s with | [_; s] ->
  let add x = if x <> "" then seeds := int_of_string x :: !seeds in
  List.iter add (String.split_on_char ' ' s)
  | _ -> assert false
let seeds = List.rev !seeds

let () = assert (input_line stdin = "")
let m1 = Map.read stdin
let m2 = Map.read stdin
let m3 = Map.read stdin
let m4 = Map.read stdin
let m5 = Map.read stdin
let m6 = Map.read stdin
let m7 = Map.read stdin
let map x = Map.apply_ranges m7 (
            Map.apply_ranges m6 (
            Map.apply_ranges m5 (
            Map.apply_ranges m4 (
            Map.apply_ranges m3 (
            Map.apply_ranges m2 (
            Map.apply_ranges m1 x))))))

let sol = ref max_int
let rec iter = function
  | [] -> ()
  | [_] -> assert false
  | start :: len :: seeds ->
      printf "%d..%d@." start (start+len-1);
      let il = map [start, len] in
      printf "%d intervals@." (List.length il);
      let m = List.fold_left (fun m (x,_) -> min m x) max_int il in
      sol := min !sol m;
      iter seeds
let () = iter seeds; printf "minimum location = %d@." !sol
