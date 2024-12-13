
open Format
open Lib

module M = Map.Make(struct type t = int * int let compare = Stdlib.compare end)

let wire1 = split_strings ~sep:',' (input_line stdin)
let wire2 = split_strings ~sep:',' (input_line stdin)

let exec (points, p, time) a =
  let dx, dy = match a.[0] with
    | 'U' -> 0, 1
    | 'D' -> 0, -1
    | 'R' -> 1, 0
    | 'L' -> -1, 0
    | _ -> assert false in
  let move (points, (x, y), time) =
    let p' = x + dx, y + dy in
    M.add p' (time+1) points, p', time+1 in
  repeat (int_of_string (String.sub a 1 (String.length a - 1))) move
    (M.add p time points, p, time)

let start = 0, 0
let exec wire = List.fold_left exec (M.empty, start, 0) wire

let points1,_,_ = exec wire1
let points2,_,_ = exec wire2

let ans = ref max_int
let merge p v1 v2 = match v1, v2 with
  | Some t1, Some t2 ->
      let t = t1 + t2 in if 0 < t && t < !ans then ans := t; None
  | _ -> None
let _ = M.merge merge points1 points2
let () = printf "%d@." !ans
