
open Lib

let read c =
  A.of_list (split_ints ~sep:',' (input_line c))

type kont =
  | Halt
  | Input of (int -> kont)
  | Output of int * (unit -> kont)

let start code =
  let code = ref (A.copy code) in
  let ensure i =
    let size = A.length !code in
    if i >= size then (
      let nsize = max (2 * size) (i + 1) in
      code := A.append !code (A.make (nsize - size) 0)
    ) in
  let get i    = ensure i; (!code).(i) in
  let set i v =  ensure i; (!code).(i) <- v in
  let base = ref 0 in
  let rec exec i =
    let instr = (get i) mod 100 in
    let mode k = ((get i) / pow 10 (1 + k)) mod 10 in
    let rebase k x = if mode k = 2 then !base + x else x in
    let param k =
      let x = get (i + k) in
      match mode k with
      | 0 (* position  *) -> get x
      | 1 (* immediate *) -> x
      | 2 (* relative  *) -> get (!base + x)
      | _ -> assert false in
    match instr with
    | 99 (* halt *)->
        Halt
    | 1 | 2 as o (* arith *) ->
        let x = param 1 and y = param 2 and z = rebase 3 (get (i + 3)) in
        set z (if o = 1 then x+y else x*y);
        exec (i + 4)
    | 3 (* input *) ->
        let x = rebase 1 (get (i + 1)) in
        Input (fun v -> set x v; exec (i + 2))
    | 4 (* output *) ->
        let x = param 1 in
        Output (x, fun () -> exec (i + 2))
    | 5 (* jump-if-true *) ->
        let x = param 1 in
        if x <> 0 then exec (param 2) else exec (i + 3)
    | 6 (* jump-if-false *) ->
        let x = param 1 in
        if x = 0 then exec (param 2) else exec (i + 3)
    | 7 (* less than *) ->
        let x = param 1 and y = param 2 and z = rebase 3 (get (i + 3)) in
        set z (iverson (x < y));
        exec (i + 4)
    | 8 (* equals *) ->
        let x = param 1 and y = param 2 and z = rebase 3 (get (i + 3)) in
        set z (iverson (x = y));
        exec (i + 4)
    | 9 (* adjusts the relative base *) ->
        let x = param 1 in
        base += x;
        exec (i + 2)
    | _ ->
        assert false
  in
  exec 0

let exec code input output =
  let rec repeat = function
  | Halt -> ()
  | Input f -> repeat (f (input ()))
  | Output (v, f) -> output v; repeat (f ())
  in
  repeat (start code)

let output1 code input =
  let exception Out of int in
  try exec code input (fun v -> raise (Out v)); assert false with Out v -> v
