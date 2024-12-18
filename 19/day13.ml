open Format
open Lib
open Machine
open Grid

let code = A.of_list (split_ints ~sep:',' (input_line stdin))

let g = make 26 45 ' '
let draw x y v =
  let c = match v with
    | 0 -> ' '
    | 1 -> '+'
    | 2 -> '#'
    | 3 -> '-'
    | 4 -> 'O'
    | _ -> assert false in
  set g (y, x) c

let input = fun _ -> 0
let output =
  let x = ref 0 and y = ref 0 in
  let step = ref 0 in
  fun v -> match !step with
           | 0 -> x := v; incr step
           | 1 -> y := v; incr step
           | _ -> draw !x !y v; step := 0

let () = exec code input output
let () = printf "%a@." print_chars g

let ans = fold (fun _ c acc -> if c = '#' then acc+1 else acc) g 0
let () = printf "%d@." ans
