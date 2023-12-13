
open Format

let s = ref 0

let matches p s i = (* s[i..] = p *)
  let lenp = String.length p in
  String.length s - i >= lenp && String.sub s i lenp = p

let patterns = [
    "0", 0; "zero", 0;
    "1", 1; "one", 1;
    "2", 2; "two", 2;
    "3", 3; "three", 3;
    "4", 4; "four", 4;
    "5", 5; "five", 5;
    "6", 6; "six", 6;
    "7", 7; "seven", 7;
    "8", 8; "eight", 8;
    "9", 9; "nine", 9;
  ]

let scan s =
  let first = ref (-1) in
  let last = ref 0 in
  let test i (p, v) =
    if matches p s i then (
      if !first = -1 then first := v;
      last := v
    ) in
  for i = 0 to String.length s - 1 do
    List.iter (test i) patterns
  done;
  10 * !first + !last

let () =
  while true do match read_line () with
  | l -> s := !s + scan l
  | exception End_of_file -> printf "sum = %d@." !s; exit 0
  done
