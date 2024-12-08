
open Format
open Grid

let g = read stdin

let start = ref (0, 0)
let () = let find p c = if c = '^' then start := p in iter find g

let turn90 = function
  | N -> E
  | E -> S
  | S -> W
  | W -> N
  | _ -> assert false

let rec visit p d =
  if inside g p then (
    assert (get g p <> '#');
    set g p 'X';
    let p' = move d p in
    if inside g p' && get g p' = '#' then visit p (turn90 d) else visit p' d
  )

let () =
  visit !start N;
  print_chars std_formatter g;
  let sum = fold (fun _ c sum -> if c = 'X' then sum+1 else sum) g 0 in
  printf "%d@." sum
