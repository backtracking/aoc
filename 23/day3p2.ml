
open Format
open Lib

let lines = input_lines stdin

let grid = Array.map array_of_string (Array.of_list lines)
let get = Grid.get grid

let height = Grid.height grid and width = Grid.width grid
let () = printf "%dx%d@." height width

let check star i j =
  let b = ref star in
  Grid.all_around grid i j (fun i' j' -> match get i' j' with
                               | '*' -> b := Some (i', j')
                               | _ -> ());
  !b

let gears = Hashtbl.create 16 (* star position -> adjacent numbers *)
let add star n = match star with
  | None -> ()
  | Some p ->
      Hashtbl.replace gears p (n :: try Hashtbl.find gears p
                                    with Not_found -> [])

let rec scan cur star i j =
  if i = height then add star cur else
  if j = width  then (add star cur; scan 0 None (i+1) 0) else
  match get i j with
  | '0'..'9' as c ->
      let cur = 10 * cur + Char.code c - Char.code '0' in
      let star = check star i j in
      scan cur star i (j+1)
  | _ ->
      add star cur;
      scan 0 None i (j+1)

let () =
  scan 0 None 0 0;
  let ratio _ l sum = match l with
    | [x;y] -> sum + x*y
    | _ -> sum in
  let sum = Hashtbl.fold ratio gears 0 in
  printf "sum = %d@." sum
