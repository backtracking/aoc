
open Format
open Lib
open Grid

let g0 = map (fun _ c -> c = '#') (read stdin)

let next g =
  init 5 5 (fun p ->
    let nn = fold4 (fun q b acc -> if b then acc+1 else acc) g p 0 in
    if get g p then nn = 1 else nn = 1 || nn = 2
  )

let biodiversity g =
  fold (fun (i, j) b acc -> if b then acc + (1 lsl (5 * i + j)) else acc) g 0

let () =
  let visited = H.create 16 in
  let rec loop g =
    printf "%a@."
      (print (fun fmt _ b -> fprintf fmt "%c" (if b then '#' else '.'))) g;
    if H.mem visited g then (
      printf "biodiversity = %d@." (biodiversity g);
      exit 0
    );
    H.add visited g ();
    loop (next g)
  in
  loop g0
