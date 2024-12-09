
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
  let files = ref [] in
  let spaces = ref [] in
  let fill c =
    let n = Char.code c - Char.code '0' in
    if not !free then (Array.fill a !next n !id;
                       files := (!next, n, !id) :: !files; incr id)
    else spaces := (!next, n) :: !spaces;
    next += n;
    free := not !free
  in
  String.iter fill s;
  (* print a; *)
  let spaces = ref (List.rev !spaces) in
  (* compacting *)
  let store (pos, len, id) =
    let rec update = function
      | [] -> []
      | (poss, lens) :: spaces when lens >= len && poss + len <= pos ->
          Array.fill a poss len id;
          Array.fill a pos  len (-1);
          if lens > len then (poss + len, lens - len) :: spaces else spaces
      | sp :: spaces -> sp :: update spaces
    in
    spaces := update !spaces
  in
  List.iter store !files;
  (* print a; *)
  (* summing *)
  let s = ref 0 in
  for i = 0 to size - 1 do
    if a.(i) <> -1 then s += i * a.(i)
  done;
  printf "sum = %d@." !s;
  ()
