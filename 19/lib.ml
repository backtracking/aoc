
let int_of_char c = Char.code c - Char.code '0'

let input_lines c =
  let rec loop acc = match input_line c with
    | s -> loop (s :: acc)
    | exception End_of_file -> List.rev acc in
  loop []

let rec fold_lines c f acc =
  match input_line c with
  | l -> fold_lines c f (f l acc)
  | exception End_of_file -> acc

let rec iter_lines c f =
  match input_line c with
  | l -> f l; iter_lines c f
  | exception End_of_file -> ()

let split_strings ?(sep=' ') s =
  let l = String.split_on_char sep s in
  List.filter ((<>) "") l

let split2 ?(sep=' ') s =
  match split_strings ~sep s with
  | [x; y] -> x, y
  | _ -> invalid_arg "split2"

let split3 ?(sep=' ') s =
  match split_strings ~sep s with
  | [x; y; z] -> x, y, z
  | _ -> invalid_arg "split2"

let split_ints ?(sep=' ') s =
  List.map int_of_string (split_strings ~sep s)

let list_of_string s =
  let rec build acc i = if i < 0 then acc else build (s.[i] :: acc) (i - 1) in
  build [] (String.length s - 1)

let array_of_string s =
  Array.init (String.length s) (String.get s)

let rec gcd a b = let m = a mod b in if m = 0 then b else gcd b m

let lcm a b =
  if a = 0 then b
  else if b = 0 then a
  else (a / gcd a b) * b

let rec forall lo hi p =
  lo >= hi || p lo && forall (lo+1) hi p

let rec sumaux f acc lo hi =
  if lo >= hi then acc else sumaux f (acc + f lo) (lo + 1) hi
let sum lo hi f =
  sumaux f 0 lo hi

let rec fold_int lo hi f acc =
  if lo >= hi then acc else fold_int (lo+1) hi f (f lo acc)

let memo ff =
  let h = Hashtbl.create 8192 in
  let rec f x =
    try Hashtbl.find h x
    with Not_found -> let v = ff f x in Hashtbl.add h x v; v
  in
  f

let sum_array a f =
  sum 0 (Array.length a) (fun i -> f i a.(i))

let (+=) r x =
  r := !r + x

let iverson b =
  if b then 1 else 0

let rec repeat n f x =
  if n = 0 then x else repeat (n-1) f (f x)

module Sint = struct
  include Set.Make(Int)
  let range lo hi = fold_int lo hi add empty
end

let eucl_div a b =
  if b <= 0 then invalid_arg "eucl_div";
  if a >= 0 then a / b, a mod b else a / b - 1, b + a mod b

module GaussianElimination(Q: sig
  type t
  val zero: t
  val equal: t -> t -> bool
  val sub: t -> t -> t
  val mul: t -> t -> t
  val div: t -> t -> t
end) = struct

  let gaussian_elimination select a =
    let a = Array.map Array.copy a in
    let n = Array.length a in
    if n = 0 then a else
    let m = Array.length a.(0) in
    for k = 0 to min n m - 1 do
      let imax = ref k in
      for i = k+1 to n - 1 do
        let x = select a.(i).(k) a.(!imax).(k) in
        if Q.equal x a.(i).(k) then imax := i
      done;
      if !imax <> k then (
        let t = a.(k) in a.(k) <- a.(!imax); a.(!imax) <- t;
      );
      let pivot = a.(k).(k) in
      if Q.equal pivot Q.zero then failwith "gaussian_elimination: singular matrix";
      for j = 0 to m - 1 do
        a.(k).(j) <- Q.div a.(k).(j) pivot
      done;
      for i = 0 to n - 1 do if i <> k then (
        let x = a.(i).(k) in
        for j = 0 to m - 1 do
          a.(i).(j) <- Q.sub a.(i).(j) (Q.mul a.(k).(j) x)
        done
      )
      done
    done;
    a

end
