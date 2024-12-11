
open Format
open Lib

let print = pp_print_list ~pp_sep:pp_print_space pp_print_int

let blink1 = function
  | 0 ->
      [1]
  | x ->
      let s = string_of_int x in
      let n = String.length s in
      if n mod 2 = 0 then
        let n = n / 2 in [ int_of_string (String.sub s 0 n)
                         ; int_of_string (String.sub s n n) ]
      else
        [x * 2024]

let count = memo (fun count (x, n) ->
  if n = 0 then 1 else
  List.fold_left (fun s x -> s + count (x, n-1)) 0 (blink1 x)
)

let stones = split_ints (input_line stdin)
let () = printf "@[%a@]@." print stones

let steps = int_of_string Sys.argv.(1)
let ans = List.fold_left (fun s x -> s + count (x, steps)) 0 stones

let () = printf "%d@." ans

