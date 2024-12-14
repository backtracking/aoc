
open Format
open Lib

let width = 25
let height = 6

let () =
  let img = A.make_matrix height width 2 in
  try while true do
    for i = 0 to height-1 do for j = 0 to width-1 do
      let c = input_char stdin in
      if c = '\n' then raise Exit;
      let c = int_of_char c in
      if c < 2 && img.(i).(j) = 2 then img.(i).(j) <- c
    done done
  done with Exit ->
    let f _ = function
      | 0 -> ' '
      | 1 -> '#'
      | 2 -> ' '
      | _ -> assert false
    in
    Grid.print_chars std_formatter (Grid.map f img)

