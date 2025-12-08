
open Lib
open Format

let points =
  let l = ref [] in
  iter_lines stdin (fun s ->
  match split_ints ~sep:',' s with
  | [x;y;z] -> l := (x,y,z) :: !l | _ -> assert false);
  Array.of_list !l

let n = Array.length points

let limit = int_of_string Sys.argv.(1)

let sqr x = x*x
let dist (x,y,z) (x',y',z') = sqr (x-x') + sqr (y-y') + sqr (z-z')

module PQ = Pqueue.MakeMin(struct
  type t = int * int * int
  let compare (d1,_,_) (d2,_,_) = Stdlib.compare d1 d2
end)

let pq = PQ.create ()

let () = points |> Array.iteri @@ fun i pi ->
         points |> Array.iteri @@ fun j pj -> if i < j then
         PQ.add pq (dist pi pj, i, j)

let uf = UF.create n

let () =
  for _ = 1 to limit do
    let d, i, j = PQ.pop_min pq |> Option.get in
    printf "%d -- %d (%d)@." i j d;
    UF.union uf i j
  done;
  let size = Array.make n 0 in
  for i = 0 to n-1 do
    let ri = UF.find uf i in size.(ri) <- 1 + size.(ri)
  done;
  Array.sort Stdlib.compare size;
  let p = size.(n-1) * size.(n-2) * size.(n-3) in
  printf "product = %d@." p

