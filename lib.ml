
module A = Array
module L = List
module H = Hashtbl

let int_of_char c = Char.code c - Char.code '0'
let char_of_int n = Char.chr (Char.code '0' + n)

let print_position fmt (i, j) = Format.fprintf fmt "%d,%d" i j

let input_lines c =
  let rec loop acc = match input_line c with
    | s -> loop (s :: acc)
    | exception End_of_file -> List.rev acc in
  loop []

let rec fold_lines c f acc =
  match input_line c with
  | l -> fold_lines c f (f l acc)
  | exception End_of_file -> acc

let rec map_lines c f =
  match input_line c with
  | l -> f l :: map_lines c f
  | exception End_of_file -> []

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

let rec pop acc x =
  if x = 0 then acc
  else let b = x land -x in pop (acc + 1) (x land lnot b)

let pop x = pop 0 x

let rec gcd a b = let m = a mod b in if m = 0 then b else gcd b m

let lcm a b =
  if a = 0 then b
  else if b = 0 then a
  else (a / gcd a b) * b

let rec extended_gcd a b =
  if a mod b = 0 then
    0, 1, b
  else
    let x,y,d = extended_gcd b (a mod b) in
    y, x - y*(a/b), d

let div_mod x y m =
  assert (y <> 0);
  let iy, _, g = extended_gcd y m in
  assert (g = 1);
  let iy = (iy + m) mod m in
  (x * iy) mod m

let inv_mod y m =
  assert (y <> 0);
  let iy, _, g = extended_gcd y m in
  assert (g = 1);
  (iy + m) mod m

let rec forall lo hi p =
  lo >= hi || p lo && forall (lo+1) hi p

let rec exists lo hi p =
  lo < hi && (p lo || exists (lo+1) hi p)

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

let (+=) r x = r := !r + x
let (-=) r x = r := !r - x

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

let rec pow x n =
  if n = 0 then
    1
  else
    let y = pow x (n/2) in
    if n mod 2 = 1 then x * y * y else y * y

let rec print_binary fmt = function
  | 0 -> Format.fprintf fmt "0"
  | 1 -> Format.fprintf fmt "1"
  | n -> Format.fprintf fmt "%a%d" print_binary (n/2) (n mod 2)

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
      if Q.equal pivot Q.zero then
        failwith "gaussian_elimination: singular matrix";
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

module Heap(X : sig
  type t
  val compare: t -> t -> int
end) = struct

  (* The heap is encoded in the array [data], where elements are stored
     from [0] to [size - 1]. From an element stored at [i], the left
     (resp. right) subtree, if any, is rooted at [2*i+1] (resp. [2*i+2]). *)

  type t = { mutable size : int; mutable data : X.t array }

  exception Empty

  (* When [create n] is called, we cannot allocate the array, since there is
     no known value of type [X.t]; we'll wait for the first addition to
     do it, and we remember this situation with a negative size. *)

  let create n =
    if n <= 0 then invalid_arg "create";
    { size = -n; data = [||] }

  let is_empty h = h.size <= 0

  (* [resize] doubles the size of [data] *)

  let resize h =
    let n = h.size in
    assert (n > 0);
    let n' = 2 * n in
    let d = h.data in
    let d' = Array.make n' d.(0) in
    Array.blit d 0 d' 0 n;
    h.data <- d'

  let add h x =
    (* first addition: we allocate the array *)
    if h.size < 0 then begin
      h.data <- Array.make (- h.size) x; h.size <- 0
    end;
    let n = h.size in
    (* resizing if needed *)
    if n == Array.length h.data then resize h;
    let d = h.data in
    (* moving [x] up in the heap *)
    let rec moveup i =
      let fi = (i - 1) / 2 in
      if i > 0 && X.compare d.(fi) x < 0 then begin
	d.(i) <- d.(fi);
	moveup fi
      end else
	d.(i) <- x
    in
    moveup n;
    h.size <- n + 1

  let maximum h =
    if h.size <= 0 then raise Empty;
    h.data.(0)

  let remove h =
    if h.size <= 0 then raise Empty;
    let n = h.size - 1 in
    h.size <- n;
    let d = h.data in
    let x = d.(n) in
    (* moving [x] down in the heap *)
    let rec movedown i =
      let j = 2 * i + 1 in
      if j < n then
	let j =
	  let j' = j + 1 in
	  if j' < n && X.compare d.(j') d.(j) > 0 then j' else j
	in
	if X.compare d.(j) x > 0 then begin
	  d.(i) <- d.(j);
	  movedown j
	end else
	  d.(i) <- x
      else
	d.(i) <- x
    in
    movedown 0

  let pop_maximum h = let m = maximum h in remove h; m

  let iter f h =
    let d = h.data in
    for i = 0 to h.size - 1 do f d.(i) done

  let fold f h x0 =
    let n = h.size in
    let d = h.data in
    let rec foldrec x i =
      if i >= n then x else foldrec (f d.(i) x) (succ i)
    in
    foldrec x0 0

end

module Modular(M : sig val m : int end) = struct

  open M

  type t = int

  let () =
    assert (m > 0);
    assert (m < 1 lsl (Sys.word_size - 3))

  let zero = 0
  let one = 1

  let of_int x =
    let r = x mod m in
    if r < 0 then r + m else r

  let add x y = (x + y) mod m
  let (++) = add

  let (++=) r x = r := (!r + x) mod m

  let rec sumaux acc lo hi f =
    if lo > hi then acc else sumaux (acc ++ f lo) (lo + 1) hi f
  let sum ~lo ~hi f =
    sumaux 0 lo hi f

  let sub x y = (m + x - y) mod m
  let (--) = sub
  let (--=) r x = r := sub !r x

  let mul1 x y = (x * y) mod m

  let mul2 x y =
    if x = 0 || y = 0 then 0 else
    let rec loopk k = if k land x <> 0 then k else loopk (k lsr 1) in
    let k = loopk (1 lsl (Sys.word_size - 3)) in
    let rec mul r k =
      if k = 0 then
	r
      else
	let r = r + r in
	let r = if r >= m then r - m else r in
	let r =
	  if x land k = 0 then
	    r
	  else
	    let r = r + y in
	    if r >= m then r - m else r
	in
	mul r (k lsr 1)
    in
    mul 0 k

  let mul = if m < 1 lsl (Sys.word_size / 2 - 1) then mul1 else mul2
  let ( ** ) = mul

  let rec prodaux acc lo hi f =
    if lo > hi then acc else prodaux (acc ** f lo) (lo + 1) hi f
  let prod ~lo ~hi f =
    prodaux 1 lo hi f

  let rec fact n =
    if n <= 1 then 1 else n ** fact (n-1)

  let fact_upto n =
    let f = Array.make (n + 1) 1 in
    for i = 1 to n do f.(i) <- i ** f.(i-1) done;
    f

  let fib = memo (fun f n -> if n <= 1 then n else f (n-2) ++ f (n-1))

  let fib_upto n =
    let f = Array.make (n + 1) 0 in
    if n > 0 then f.(1) <- 1;
    for i = 2 to n do f.(i) <- f.(i-2) ++ f.(i-1) done;
    f

  let rec power x n =
    if n = 0 then
      1
    else
      let y = power x (n/2) in
      if n mod 2 = 1 then mul x (mul y y) else mul y y

  let div x y =
    assert (y <> 0);
    let iy, _, g = extended_gcd y m in
    assert (g = 1);
    let iy = (iy + m) mod m in
    x ** iy

  let ( // ) = div

  let print = Format.pp_print_int

  type factorials = { fact: int array; inv_fact: int array }

  let factorials_upto limit =
    let fact = Array.make (limit + 1) 1 in
    for i = 2 to limit do fact.(i) <- i ** fact.(i-1) done;
    let inv_fact = Array.init (limit + 1) (fun i -> 1 // fact.(i)) in
    { fact = fact; inv_fact = inv_fact }

  let choose f n k =
    if k = 0 || k = n then 1 else if k > n then 0 else
        f.fact.(n) ** f.inv_fact.(k) ** f.inv_fact.(n - k)

  let cnk n k =
    if k > n then 0 else
      let k = min (n - k) k in
      let rec loop acc i =
        if i > k then acc else loop (acc ** (n - i + 1) // i) (i + 1) in
      loop 1 1
end
