
open Lib
open Format

let points =
  let add s acc = match split_ints ~sep:',' s with
    | [x; y] -> (x, y) :: acc
    | _ -> assert false in
  fold_lines stdin add [] |> Array.of_list

let m = ref 0
let () = points |> Array.iteri @@ fun i (x1,y1) ->
  for j = i+1 to Array.length points -1 do
    let x2,y2 = points.(j) in
    let a = (1 + abs (x1 - x2)) * (1 + abs (y1 - y2)) in
    if a > !m then m := a
  done

let () = printf "max area = %d@." !m
