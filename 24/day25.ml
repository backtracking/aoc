
open Format
open Lib

let locks = ref []
let keys  = ref []

let read () =
  let s = input_line stdin in
  let lock = s = "#####" in
  let a = A.make 5 0 in
  for _ = 1 to 5 do
    let s = input_line stdin in
    String.iteri (fun i c -> if c = '#' then a.(i) <- a.(i) + 1) s
  done;
  assert (input_line stdin = if lock then "....." else "#####");
  if lock then locks := a :: !locks else keys := a :: !keys;
  assert (input_line stdin = "")

let () = try while true do read () done with End_of_file -> ()
let () = printf "%d locks and %d keys@."
           (List.length !locks) (List.length !keys)

let fit lo ke = forall 0 5 (fun i -> lo.(i) + ke.(i) <= 5)

let () =
  let ans = ref 0 in
  !locks |> List.iter (fun lo ->
  !keys  |> List.iter (fun ke ->
    if fit lo ke then incr ans
  ));
  printf "%d@." !ans
