
open Format
open Grid
module H = Hashtbl

let g = read stdin

let antennas = H.create 16 (* letter -> positions *)
let () =
  let add p c =
    if c <> '.' then
    let l = p :: try H.find antennas c with Not_found -> [] in
    H.replace antennas c l in
  iter add g

let antinodes = H.create 16

let pair (x1, y1 as p1) (x2, y2 as p2) =
  assert (p1 <> p2);
  let test p =
    if inside g p then (set g p '#'; H.replace antinodes p ()) in
  test (2 * x1 - x2, 2 * y1 - y2);
  test (2 * x2 - x1, 2 * y2 - y1)

let rec iter_pair f = function
  | [] | [_] -> ()
  | x :: l -> List.iter (f x) l; iter_pair f l

let compute _ pl =
  iter_pair pair pl

let () = H.iter compute antennas
let () = printf "%a@." print_chars g
let () = printf "%d@." (H.length antinodes)


