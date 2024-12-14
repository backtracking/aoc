
open Format
open Lib
open Machine

let code = A.of_list (split_ints ~sep:',' (input_line stdin))

let phase = A.make 5 0

let ans = ref 0

let run () =
  for i = 0 to 4 do printf "%d," phase.(i); done; printf " => @?";
  let amp = A.init 5 (fun _ -> start (A.copy code)) in
  Array.iteri (fun i k -> match k with
                          | Input f -> amp.(i) <- f phase.(i)
                          | Halt | Output _ -> assert false) amp;
  let last = ref 0 in
  let rec loop () =
    Array.iteri (fun i k -> match k with
     | Input f -> (match f !last with
                   | Output (v, f) -> last := v; amp.(i) <- f ();
                                      if i = 4 && v > !ans then ans := v
                   | Input _ -> assert false
                   | Halt -> ())
     | Halt | Output _ -> assert false
    ) amp;
    if exists 0 5 (fun i -> amp.(i) <> Halt) then loop ()
  in
  loop ();
  printf "%d@." !last

let rec test i =
  if i = 5 then run () else
  for v = 5 to 9 do
    if forall 0 i (fun j -> phase.(j) <> v) then (phase.(i) <- v; test (i + 1))
  done

let () = test 0; printf "%d@." !ans
