
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

let cache = Array.init 3 (fun _ -> H.create 16)
let todo = Array.make 3 true
let copy d = (Array.init 4 (fun i -> moons.(i).p.(d)),
              Array.init 4 (fun i -> moons.(i).v.(d)))

let ans = ref 1

let () =
  let s = ref 0 in
  while true do
    let store d =
      if todo.(d) then
      let m = copy d in
      if H.mem cache.(d) m then (
        let last = H.find cache.(d) m in
        let len = !s - last in
        printf "dimension %d repeats: step %d = step %d (len = %d)@."
          d !s last len;
        ans := lcm !ans len;
        printf "lcm = %d@." !ans;
        todo.(d) <- false;
        if forall 0 3 (fun i -> not todo.(i)) then exit 0
      ) else
        H.add cache.(d) m !s
    in
    for d = 0 to 2 do store d done;
    step ();
    incr s
  done

(*
lcm = 551272644867044
*)
