
open Format
open Lib
open Grid

let g0 = map (fun _ c -> c = '#') (read stdin)

(*
 0,0 | 0,1 |   0,2   | 0,3 | 0,4
     |     |         |     |
-----+-----+---------+-----+-----
     |     |         |     |
 1,0 | 1,1 |   1,2   | 1,3 | 1,4
     |     |         |     |
-----+-----+---------+-----+-----
     |     |A|B|C|D|E|     |
     |     |-+-+-+-+-|     |
     |     |F|G|H|I|J|     |
     |     |-+-+-+-+-|     |
 2,0 | 2,1 |K|L|?|N|O| 2,3 | 2,4
     |     |-+-+-+-+-|     |
     |     |P|Q|R|S|T|     |
     |     |-+-+-+-+-|     |
     |     |U|V|W|X|Y|     |
-----+-----+---------+-----+-----
     |     |         |     |
 3,0 | 3,1 |   3,2   | 3,3 | 3,4
     |     |         |     |
-----+-----+---------+-----+-----
     |     |         |     |
 4,0 | 4,1 |   4,2   | 4,3 | 4,4
     |     |         |     |
*)

let next inn g out =
  let inn i j = get inn (i, j) and
      g   i j = get g   (i, j) and
      out i j = get out (i, j) in
  let nb = function
    | 0, 0 -> [g 0 1; g 1 0;        out 1 2; out 2 1]
    | 0, 1 -> [g 0 0; g 0 2; g 1 1; out 1 2         ]
    | 0, 2 -> [g 0 1; g 0 3; g 1 2; out 1 2         ]
    | 0, 3 -> [g 0 2; g 0 4; g 1 3; out 1 2         ]
    | 0, 4 -> [g 0 3; g 1 4;        out 1 2; out 2 3]
    | 1, 0 -> [g 0 0; g 1 1; g 2 0; out 2 1]
    | 1, 1 -> [g 1 0; g 0 1; g 1 2; g 2 1]
    | 1, 2 -> [g 1 1; g 0 2; g 1 3; inn 0 0; inn 0 1; inn 0 2; inn 0 3; inn 0 4]
    | 1, 3 -> [g 1 2; g 0 3; g 1 4; g 2 3]
    | 1, 4 -> [g 1 3; g 0 4; g 2 4; out 2 3]
    | 2, 0 -> [g 1 0; g 2 1; g 3 0; out 2 1]
    | 2, 1 -> [g 2 0; g 1 1; g 3 1; inn 0 0; inn 1 0; inn 2 0; inn 3 0; inn 4 0]
    | 2, 2 -> [] (* not meaningful *)
    | 2, 3 -> [g 1 3; g 2 4; g 3 3; inn 0 4; inn 1 4; inn 2 4; inn 3 4; inn 4 4]
    | 2, 4 -> [g 2 3; g 1 4; g 3 4; out 2 3]
    | 3, 0 -> [g 2 0; g 3 1; g 4 0; out 2 1]
    | 3, 1 -> [g 3 0; g 2 1; g 3 2; g 4 1]
    | 3, 2 -> [g 3 1; g 4 2; g 3 3; inn 4 0; inn 4 1; inn 4 2; inn 4 3; inn 4 4]
    | 3, 3 -> [g 3 2; g 2 3; g 3 4; g 4 3]
    | 3, 4 -> [g 3 3; g 2 4; g 4 4; out 2 3]
    | 4, 0 -> [g 3 0; g 4 1; out 2 1; out 3 2]
    | 4, 1 -> [g 4 0; g 3 1; g 4 2; out 3 2]
    | 4, 2 -> [g 4 1; g 3 2; g 4 3; out 3 2]
    | 4, 3 -> [g 4 2; g 3 3; g 4 4; out 3 2]
    | 4, 4 -> [g 4 3; g 3 4; out 3 2; out 2 3]
    | _ -> assert false in
  init 5 5 (fun (i,j as p) ->
    let nn = List.fold_left (fun acc b -> if b then acc+1 else acc) 0 (nb p) in
    if g i j then nn = 1 else nn = 1 || nn = 2
  )

let minutes = int_of_string Sys.argv.(1)
let size = 2 * minutes + 1
let empty () = make 5 5 false

let () =
  let a = ref (A.init size (fun i -> if i = minutes then g0 else empty ())) in
  for _ = 1 to minutes do
    let compute i = if i = 0 || i = size - 1 then empty ()
                    else next !a.(i-1) !a.(i) !a.(i+1) in
    let b = A.init size compute in
    a := b
  done;
  for i = 0 to size - 1 do
    printf "Depth %d:@." (i - minutes);
    printf "%a@."
      (print (fun fmt _ b -> fprintf fmt "%c" (if b then '#' else '.'))) !a.(i);
  done;
  let count acc g = fold (fun _ b acc -> if b then acc+1 else acc) g acc in
  let tot = A.fold_left count 0 !a in
  printf "%d bugs@." tot
