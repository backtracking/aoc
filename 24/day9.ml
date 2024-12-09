
open Format
open Lib

let s = input_line stdin
let () = printf "%d characters@." (String.length s)
let size = String.fold_left (fun s c -> s + Char.code c - Char.code '0') 0 s
let () = printf "size = %d@." size

let print a =
  for i = 0 to size - 1 do
    printf "%c" (if a.(i) = -1 then '.' else Char.chr (Char.code '0' + a.(i)))
  done;
  printf "@."

let () =
  let a = Array.make size (-1) in
  let id = ref 0 in
  let free = ref false in
  let next = ref 0 in
  let fill c =
    let n = Char.code c - Char.code '0' in
    if not !free then (Array.fill a !next n !id; incr id);
    next += n;
    free := not !free
  in
  String.iter fill s;
  (* print a; *)
  (* compacting *)
  let last = ref (size - 1) in
  let next = ref 0 in
  while a.(!next) <> -1 do incr next done;
  while !next < !last do
    a.(!next) <- a.(!last);
    a.(!last) <- -1;
    decr last;
    while a.(!next) <> -1 do incr next done;
  done;
  (* print a; *)
  (* summing *)
  let s = ref 0 in
  for i = 0 to !last do
    s += i * a.(i)
  done;
  printf "sum = %d@." !s;
  ()

