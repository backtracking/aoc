
open Format

let s = ref 0

let scan s =
  let first = ref (-1) in
  let last = ref 0 in
  let add = function
    | '0'..'9' as c ->
        let v = Char.code c - Char.code '0' in
        if !first = -1 then first := v; last := v
    | _ -> () in
  String.iter add s;
  10 * !first + !last

let () =
  while true do match read_line () with
  | l -> s := !s + scan l
  | exception End_of_file -> printf "sum = %d@." !s; exit 0
  done
