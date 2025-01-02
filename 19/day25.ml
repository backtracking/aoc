
open Format
open Lib
open Machine

let code = read_from_file "input25.txt"

let q = Queue.create ()

let shortcut = function
  | "n" -> "north"
  | "s" -> "south"
  | "w" -> "west"
  | "e" -> "east"
  | s   -> s

let rec input () =
  if Queue.is_empty q then (
    let s = read_line () in
    let s = shortcut s in
    String.iter (fun c -> Queue.add c q) s;
    Queue.add '\n' q;
    input ()
  ) else
    Char.code (Queue.pop q)

let output v = output_char stdout (Char.chr v)

let () = exec code input output
