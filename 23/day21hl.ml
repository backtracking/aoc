
open Format
open Lib
open Grid

let g = read stdin
let h = height g and w = width g
let () = printf "%dx%d@." h w

let start = find (fun _ c -> c = 'S') g
let () = printf "start = %d,%d@." (fst start) (snd start)
let () = set g start '.'

(* Hashlife *)

type q =
  { id:int; lvl: int; tree: tree; count: int }
and tree =
  | Rock (*#*)
  | Plot (*.*)
  | Step (*O*)
  | Q of q * q * q * q

module HC = Hashtbl.Make(struct
  type t = tree
  let hash = function
    | Rock -> 0
    | Plot -> 1
    | Step -> 2
    | Q (a,b,c,d) -> 31 * (31 * (31 * c.id + d.id) + b.id) + a.id

  let equal t1 t2 = match t1, t2 with
    | Rock, Rock | Plot, Plot | Step, Step -> true
    | Q (a,b,c,d), Q (x,y,z,t) -> a==x && b==y && c==z && d==t
    | _ -> false
end)

let count = function
  | Rock | Plot -> 0
  | Step -> 1
  | Q (a,b,c,d) -> a.count + b.count + c.count + d.count

let id = ref 0
let table = HC.create 8192
let hashcons (t: tree) : q =
  try
    HC.find table t
  with Not_found ->
    incr id;
    let lvl = match t with
    | Rock | Plot | Step -> 0
    | Q (a,b,c,d) ->
      assert (b.lvl=a.lvl && c.lvl=a.lvl && d.lvl=a.lvl); a.lvl + 1 in
    let q = { id = !id; lvl; tree = t; count = count t } in
    HC.add table t q;
    q

let rock = hashcons Rock
let plot = hashcons Plot
let step = hashcons Step
let mk (a,b,c,d) = hashcons (Q (a,b,c,d))

module M = struct
  include Hashtbl.Make(struct
    type t = q
    let equal (x: q) (y: q) = x==y
    let hash q = q.id
  end)
  let memo ff =
    let h = create 8192 in
    let rec f x =
      try find h x
      with Not_found -> let v = ff f x in add h x v; v
    in
    f
end

(*** debug printer (was useful) ******************************)
let rec qget (i, j) q = match q.tree with
  | Rock -> '#'
  | Plot -> '.'
  | Step -> 'O'
  | Q (a,b,c,d) ->
      let size = 1 lsl (q.lvl - 1) in
      if i < size then
        if j < size then qget (i, j) a else qget (i, j-size) b
      else
        if j < size then qget (i-size, j) c else qget (i-size, j-size) d

let print q =
  let size = 1 lsl q.lvl in
  for i = 0 to size - 1 do
    for j = 0 to size - 1 do
      printf "%c" (qget (i, j) q)
    done;
    printf "@."
  done
(*************************************************************)

(* build the initial quad tree from the map *)
let get i j =
  let i = i mod h in let i = if i < 0 then i + h else i in
  let j = j mod w in let j = if j < 0 then j + w else j in
  get g (i,j)

let rec build i j lvl =
  if lvl = 0 then
    if (i,j) = start then step else
    match get i j with '#' -> rock | '.' -> plot | _ -> assert false
  else
    let lvl = lvl - 1 in
    let size = 1 lsl lvl in
    mk (build i        j lvl, build i        (j+size) lvl,
        build (i+size) j lvl, build (i+size) (j+size) lvl)

let rec log2 n = if n = 1 then 0 else 1 + log2 (n / 2)
let () = printf "init lvl = %d@." (1 + log2 w)

let init = build 0 0 (1 + log2 w)
let () = assert (init.count = 1)
let () = assert (qget start init = 'O')

(* double the size of a quad tree, with the map around but no steps *)
let build = memo (fun build (i, j, lvl) ->
  let i = i mod h in let i = if i < 0 then i + h else i in
  let j = j mod w in let j = if j < 0 then j + w else j in
  if lvl = 0 then
    match get i j with '#' -> rock | '.' -> plot | _ -> assert false
  else
    let lvl = lvl - 1 in
    let size = 1 lsl lvl in
    mk (build (i,      j, lvl), build (i,      j+size, lvl),
        build (i+size, j, lvl), build (i+size, j+size, lvl))
)

let enlarge = M.memo (fun _ q ->
  assert (q.lvl > 0);
  let i = - (1 lsl q.lvl) + 1 lsl (init.lvl - 1) in
  let j = i in
  let l = q.lvl - 1 in
  let s = 1 lsl l in
  match q.tree with
    | Q(a,b,c,d) -> mk
       (mk (build (i,j,l), build (i,j+s,l), build (i+s,j,l), a),
        mk (build (i,j+2*s,l), build (i,j+3*s,l), b, build(i+s,j+3*s,l)),
        mk (build (i+2*s,j,l), c, build (i+3*s,j,l), build (i+3*s,j+s,l)),
        mk (d, build (i+2*s,j+3*s,l), build (i+3*s,j+2*s,l), build (i+3*s,j+3*s,l))
       )
    | _ -> assert false
)

(* make the initial quad tree large enough *)
let q0 = repeat 30 enlarge init
let () = assert (q0.count = 1)

(* Hashlife algorithm *)

(* compute the result for q with neighbors a,b,c,d *)
let one q a b c d =
  if q == rock then rock else
  match a.tree, b.tree, c.tree, d.tree with
  | Step,_,_,_ | _,Step,_,_ | _,_,Step,_ | _,_,_,Step -> step
  | _ -> plot

let split4 q = match q.tree with
  | Q (a,b,c,d) -> a,b,c,d
  | _ -> assert false

let split16 = M.memo (fun _ q ->
  assert (q.lvl >= 2);
  match q.tree with
  | Q (({tree=Q(q00, q01, q10, q11)}), ({tree=Q(q02, q03, q12, q13)}),
       ({tree=Q(q20, q21, q30, q31)}), ({tree=Q(q22, q23, q32, q33)})) ->
      q00, q01, q02, q03,
      q10, q11, q12, q13,
      q20, q21, q22, q23,
      q30, q31, q32, q33
  | _ ->
      assert false
)

let mk16 q00 q01 q02 q03
         q10 q11 q12 q13
         q20 q21 q22 q23
         q30 q31 q32 q33 =
  mk (mk (q00, q01, q10, q11), mk (q02, q03, q12, q13),
      mk (q20, q21, q30, q31), mk (q22, q23, q32, q33))

(* advance 1 step in the future
   (we can repeat this if we have a small number of steps,
   but this is not efficient enough for 26501365 steps) *)
let next = M.memo (fun next q ->
  assert (q.lvl >= 2);
  match q.tree with
  | _ when q.lvl = 2 ->
      let q00, q01, q02, q03, q10, q11, q12, q13,
          q20, q21, q22, q23, q30, q31, q32, q33 = split16 q in
      mk (one q11 q01 q10 q21 q12,
          one q12 q02 q11 q22 q13,
          one q21 q11 q20 q31 q22,
          one q22 q12 q21 q32 q23)
  | Q (a,b,c,d) ->
      assert (q.lvl > 2);
       let a00, a01, a02, a03, a10, a11, a12, a13,
           a20, a21, a22, a23, a30, a31, a32, a33 = split16 a in
       let b00, b01, b02, b03, b10, b11, b12, b13,
           b20, b21, b22, b23, b30, b31, b32, b33 = split16 b in
       let c00, c01, c02, c03, c10, c11, c12, c13,
           c20, c21, c22, c23, c30, c31, c32, c33 = split16 c in
       let d00, d01, d02, d03, d10, d11, d12, d13,
           d20, d21, d22, d23, d30, d31, d32, d33 = split16 d in
       let a' = mk16 a11 a12 a13 b10
                     a21 a22 a23 b20
                     a31 a32 a33 b30
                     c01 c02 c03 d00 in
       let b' = mk16 a13 b10 b11 b12
                     a23 b20 b21 b22
                     a33 b30 b31 b32
                     c03 d00 d01 d02 in
       let c' = mk16 a31 a32 a33 b30
                     c01 c02 c03 d00
                     c11 c12 c13 d10
                     c21 c22 c23 d20 in
       let d' = mk16 a33 b30 b31 b32
                     c03 d00 d01 d02
                     c13 d10 d11 d12
                     c23 d20 d21 d22 in
      mk (next a', next b', next c', next d')
  | _ ->
      assert false
)

(* advance 2^(lvl-2) steps in the future *)
let result = M.memo (fun result q ->
  assert (q.lvl >= 2);
  if q.lvl = 2 then
    next q
  else
    let a,b,c,d = split4 q in
    let q00, q01, q02, q03, q10, q11, q12, q13,
        q20, q21, q22, q23, q30, q31, q32, q33 = split16 q in
    let r00 = result a in
    let r01 = result (mk (q01, q02, q11, q12)) in
    let r02 = result b in
    let r10 = result (mk (q10, q11, q20, q21)) in
    let r11 = result (mk (q11, q12, q21, q22)) in
    let r12 = result (mk (q12, q13, q22, q23)) in
    let r20 = result c in
    let r21 = result (mk (q21, q22, q31, q32)) in
    let r22 = result d in
    let a' = result (mk (r00, r01, r10, r11)) in
    let b' = result (mk (r01, r02, r11, r12)) in
    let c' = result (mk (r10, r11, r20, r21)) in
    let d' = result (mk (r11, r12, r21, r22)) in
    mk (a',b',c',d')
)

(* advance e steps in the future, provided e <= q.lvl-2 *)
let future = memo (fun future e -> M.memo (fun _ q ->
  assert (q.lvl >= 2 && e <= q.lvl-2);
  if e = q.lvl - 2 then
    result q
  else
    let a,b,c,d = split4 q in
    let q00, q01, q02, q03, q10, q11, q12, q13,
        q20, q21, q22, q23, q30, q31, q32, q33 = split16 q in
    let r00 = future e a in
    let r01 = future e (mk (q01, q02, q11, q12)) in
    let r02 = future e b in
    let r10 = future e (mk (q10, q11, q20, q21)) in
    let r11 = future e (mk (q11, q12, q21, q22)) in
    let r12 = future e (mk (q12, q13, q22, q23)) in
    let r20 = future e c in
    let r21 = future e (mk (q21, q22, q31, q32)) in
    let r22 = future e d in
    let build q00 q01 q10 q11 =
      let _,_,_,a = split4 q00 in let _,_,b,_ = split4 q01 in
      let _,c,_,_ = split4 q10 in let d,_,_,_ = split4 q11 in
      mk (a,b,c,d) in
    let a' = build r00 r01 r10 r11 in
    let b' = build r01 r02 r11 r12 in
    let c' = build r10 r11 r20 r21 in
    let d' = build r11 r12 r21 r22 in
    mk (a',b',c',d')
))

let nbsteps = int_of_string Sys.argv.(1)

(* finally, decompose the given number of steps into powers of two *)
let rec repeat n x =
  if n = 0 then x else (
  assert (x.lvl >= 2);
  assert (n < 1 lsl (x.lvl-2));
  let rec find e p = if 2*p > n then e,p else find (e+1) (2*p) in
  let e,p = find 0 1 in (* p=2^e <= n *)
  assert (e <= x.lvl - 2 );
  assert (p <= n);
  repeat (n-p) (future e x |> enlarge)
  )

let q = repeat nbsteps q0
let () = printf "%d plots@?" q.count

(*
131x131
start = 65,65
init lvl = 8
605492675373144 plots
real	2m15.613s
user	2m13.148s
sys	0m2.457s
*)
