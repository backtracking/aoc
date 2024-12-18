
open Format
open Lib

type moon = {
  p: int array;
  v: int array;
}

let read s = Scanf.sscanf s "<x=%d, y=%d, z=%d>" @@ fun x y z ->
  { p = [| x; y; z |]; v = [| 0; 0; 0 |] }

let moons = Array.init 4 (fun _ -> read (input_line stdin))

let print () =
  let print fmt v = fprintf fmt "<x=%3d, y=%3d, z=%3d>" v.(0) v.(1) v.(2) in
  let print m = printf "pos=%a, vel=%a@." print m.p print m.v in
  Array.iter print moons

let step () =
  let gravity m1 m2 =
    for j = 0 to 2 do
      let p1 = m1.p.(j) and p2 = m2.p.(j) in
      if p1 < p2 then (m1.v.(j) <- m1.v.(j) + 1; m2.v.(j) <- m2.v.(j) - 1) else
      if p1 > p2 then (m1.v.(j) <- m1.v.(j) - 1; m2.v.(j) <- m2.v.(j) + 1)
    done in
  for i = 0 to 3 do for j = i+1 to 3 do gravity moons.(i) moons.(j) done done;
  for i = 0 to 3 do let m = moons.(i) in
    for j = 0 to 2 do m.p.(j) <- m.p.(j) + m.v.(j) done
  done

let nsteps = int_of_string Sys.argv.(1)

let () =
  print ();
  for s = 1 to nsteps do
    step ();
    printf "After %d steps:@." s;
    print ()
  done;
  let sum v = abs v.(0) + abs v.(1) + abs v.(2) in
  let energy m = sum m.p * sum m.v in
  let tot = Lib.sum 0 4 (fun i -> energy moons.(i)) in
  printf "total energy: %d@." tot
