
open Format
open Lib

let size   = 119_315_717_514_047
(* let size = int_of_string Sys.argv.(1) *)
let repeat = 101_741_582_076_661

type op =
  | DealIntoNew
  | Cut of int
  | DealWithIncrement of int

let parse s =
  match s with
  | s when String.starts_with ~prefix:"cut" s ->
      Scanf.sscanf s "cut %d" (fun n -> Cut n)
  | s when String.starts_with ~prefix:"deal with increment" s ->
      Scanf.sscanf s "deal with increment %d" (fun n -> DealWithIncrement n)
  | "deal into new stack" -> DealIntoNew
  | _ -> assert false

let ops = map_lines stdin parse
let () = printf "%d operations@." (List.length ops)
let revops = List.rev ops

let div_mod x y m = Z.(
  let g, vy, _ = gcdext (of_int y) (of_int m) in
  assert (g = one);
  let i = erem (mul (of_int x) vy) (of_int m) in
  to_int i
)

let inverse i = function
  | DealIntoNew ->
      size - 1 - i
  | Cut n ->
      let n = if n < 0 then size + n else n in
      if i >= size - n then i - (size - n) else i + n
  | DealWithIncrement n ->
      let k = div_mod i n size in
      (* printf "incr n=%d i=%d k=%d@." n i k; *)
      assert (0 <= k && k < size);
      k

let inverse_all i =
  List.fold_left inverse i revops

let target = 2020
(* let target = int_of_string Sys.argv.(2) *)

let finish i step len =
  printf "cycle of length %d at step %d@." len (step - len);
  let todo = repeat - step in
  let n = todo mod len in
  let i = Lib.repeat n inverse_all i in
  printf "i = %d@." i;
  exit 0

let () =
  let seen = H.create 16 in
  let i = ref target in
  let step = ref 0 in
  while true do
    if H.mem seen !i then finish !i !step (!step - H.find seen !i);
    H.add seen !i !step;
    i := inverse_all !i;
    incr step
  done
