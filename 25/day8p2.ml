
open Lib
open Format

let points =
  let l = ref [] in
  iter_lines stdin (fun s ->
  match split_ints ~sep:',' s with
  | [x;y;z] -> l := (x,y,z) :: !l | _ -> assert false);
  Array.of_list !l

let n = Array.length points

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

let () =
  let uf = UF.create n in
  while UF.num_classes uf > 1 do
    let d, i, j = PQ.pop_min pq |> Option.get in
    printf "%d -- %d (%d)@." i j d;
    let x1,_,_ = points.(i) and x2,_,_ = points.(j) in
    if UF.find uf i <> UF.find uf j then (
      printf "  connect, x1*x2 = %d@." (x1 * x2);
      UF.union uf i j
    )
  done
