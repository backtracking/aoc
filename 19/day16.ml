
open Format
open Lib

let nphases = int_of_string Sys.argv.(1)

let s = input_line stdin
let size = String.length s
let () = printf "length %d@." size
let input = A.init size (fun i -> int_of_char s.[i])

let print fmt a =
  for i = 0 to A.length a - 1 do fprintf fmt "%d," a.(i) done

let pat = [| 0; 1; 0; -1 |]

let p = memo (fun _ (i, j) ->
  let j = j+1 in (* shift one left *)
  pat.((j / (i+1)) mod 4)
)

let next step a =
  printf "step %d@." step;
  printf "  input = %a@." print a;
  let compute i =
    (* printf "  compute i=%d with pattern %a => " *)
    (*   i print (A.init size (fun j -> p (i,j))); *)
    let v =
      fold_int 0 size (fun j s -> (s + a.(j) * p (i, j))) 0 in
    (* printf "%d@." v; *)
    abs (v mod 10) in
  let b = A.init size compute in
  printf "  output = %a@." print b;
  b

let () =
  let a = ref input in
  for step = 1 to nphases do a := next step !a done;
  printf "%a@." print !a

