
open Format
open Lib
open Machine

let code = read stdin

let size = 50

let input1 k f = match k with Input k -> f k | _ -> assert false
let output1 k = match k () with Output (v, k) -> v, k | _ -> assert false

let comp = A.init size (fun i -> start (A.copy code))
let () = for i = 0 to size - 1 do
           comp.(i) <- input1 comp.(i) (fun k -> k i)
         done

let recv = A.init size (fun _ -> Queue.create ())

let natx = ref 0
let naty = ref 0

let is_input = function Input _ -> true | _ -> false
let idle () =
  forall 0 size (fun i -> Queue.is_empty recv.(i) && is_input comp.(i))

let check =
  let seen = H.create 16 in
  fun y ->
    if H.mem seen y then (printf "Y = %d@." y; exit 0);
    H.add seen y ()

let () =
  while true do
    if idle () then (
      Queue.add !natx recv.(0);
      Queue.add !naty recv.(0);
      check !naty
    ) else
    for i = 0 to size - 1 do match comp.(i) with
    | Input k ->
        comp.(i) <-
          if Queue.is_empty recv.(i) then k (-1)
          else input1 (k (Queue.pop recv.(i))) (fun k -> k (Queue.pop recv.(i)))
    | Output (v, k) when v = 255 ->
        let x, k = output1 k in
        natx := x;
        let y, k = output1 k in
        naty := y;
        comp.(i) <- k ()
    | Output (j, k) ->
        let q = recv.(j) in
        let x, k = output1 k in
        Queue.add x q;
        let y, k = output1 k in
        Queue.add y q;
        comp.(i) <- k ()
    | Halt ->
        printf "%d halts@." i
    done
  done
