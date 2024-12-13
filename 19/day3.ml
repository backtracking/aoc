
open Format
open Lib

module S = Set.Make(struct type t = int * int let compare = Stdlib.compare end)

let wire1 = split_strings ~sep:',' (input_line stdin)
let wire2 = split_strings ~sep:',' (input_line stdin)

let points = H.create 16

let exec (points, p) a =
  let dx, dy = match a.[0] with
    | 'U' -> 0, 1
    | 'D' -> 0, -1
    | 'R' -> 1, 0
    | 'L' -> -1, 0
    | _ -> assert false in
  let move (points, (x, y)) =
    let p' = x + dx, y + dy in
    S.add p' points, p' in
  repeat (int_of_string (String.sub a 1 (String.length a - 1))) move
    (S.add p points, p)

let start = 0, 0
let exec wire = fst (List.fold_left exec (S.empty, start) wire)

let points1 = exec wire1
let points2 = exec wire2
let points = S.inter points1 points2
let ans = S.fold (fun (x, y) acc ->
              let d = abs x + abs y in if 0 < d && d < acc then d else acc)
            points max_int
let () = printf "%d@." ans
