open Format
open Lib
open Grid
open Machine

let code = read stdin

let probe x y =
  let input = let first = ref true in
              fun () -> if !first then (first := false; x) else y in
  output1 code input

let g = init 20 20 (fun (y, x) -> if probe x y = 0 then '.' else '#')
let () = printf "%a@." print_chars g

let () =
  let span = Dynarray.of_list [0,0; 1,1; 2,2; 3,3] in
  let y = ref 3 and xmin = ref 3 and xmax = ref 3 in
  while true do
    incr y;
    printf "y=%d: @?" !y;
    (* find borders of line y *)
    while probe !xmin !y = 0 do incr xmin done;
    xmax := max !xmin !xmax;
    while probe !xmax !y = 1 do incr xmax done;
    decr xmax;
    printf "%d..%d@." !xmin !xmax;
    Dynarray.add_last span (!xmin, !xmax);
    (* does the 100x100 square fits on (bottom) line y? *)
    if !y >= 99 && !xmax - !xmin + 1 >= 100 then
      let x1, x2 = Dynarray.get span (!y - 99) in
      if x1 <= !xmin && !xmin + 99 <= x2 then
        (let x = !xmin and y = !y - 99 in
         printf "top = %d, %d => %d@." x y (x * 10000 + y);
         exit 0)
  done


