open Format
open Lib
open Machine
open Grid

let interactive = Sys.argv.(1) = "play"

let () = if interactive then Graphics.open_graph " 1x1"

let code = A.of_list (split_ints ~sep:',' (input_line stdin))
let () = code.(0) <- 2 (* play *)

let bx = ref 0
let by = ref 0
let paddle = ref 0

let g = make 26 45 ' '
let draw x y v =
  let c = match v with
    | 0 -> printf "erase %d, %d@." y x; ' '
    | 1 -> '+'
    | 2 -> '#'
    | 3 -> paddle := x; 'T'
    | 4 -> by := y; bx := x; 'O'
    | _ -> assert false in
  set g (y, x) c

let score = ref 0

let input = fun _ ->
  printf "%a@." print_chars g;
  if interactive then match Graphics.read_key () with
  | 'o' -> -1
  | 'p' -> 1
  | 'q' -> printf "score = %d@."  !score; exit 0
  | _ -> 0
  else
  if !paddle < !bx then 1
  else if !paddle > !bx then -1
  else 0

let output =
  let x = ref 0 and y = ref 0 in
  let step = ref 0 in
  fun v -> match !step with
           | 0 -> x := v; incr step
           | 1 -> y := v; incr step
           | _ -> if !x = -1 && !y = 0 then (
                    printf "score += %d" (v - !score);
                    score := v; printf " => %d@." v
                  ) else draw !x !y v; step := 0

let () = exec code input output
