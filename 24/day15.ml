
open Format
open Lib
open Grid

let g =
  let rec scan rows = match input_line stdin with
    | "" ->
        let row s = Array.init (String.length s) (String.get s) in
        Array.map row (Array.of_list (List.rev rows))
    | s -> scan (s :: rows)
  in
  scan []

let () = printf "%dx%d@." (height g) (width g)

let robot = ref (find (fun _ c -> c = '@') g)
let () = set g !robot '.'

let direction = function
  | '<' -> W
  | '>' -> E
  | '^' -> N
  | 'v' -> S
  | _ -> assert false

let move d =
  let p = move d !robot in
  match get g p with
  | '#' -> ()
  | '.' -> robot := p
  | 'O' ->
      let q = ref p in while get g !q = 'O' do q := move d !q done;
      if get g !q = '.' then (
        robot := p; set g p '.'; set g !q 'O'
      )
  | _ -> assert false

let () =
  try while true do match input_char stdin with
  | '\n' -> ()
  | '<' | '>' | '^' | 'v' as c -> move (direction c)
  | _ -> assert false
  done with End_of_file ->
  let ans =
    fold (fun (i,j) c acc -> if c = 'O' then acc + 100*i+j else acc) g 0 in
  printf "%d@." ans


