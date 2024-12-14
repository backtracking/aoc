
open Lib

let exec code input output =
  let rec exec i =
    let instr = code.(i) mod 100 in
    let mode k = (code.(i) / pow 10 (1 + k)) mod 10 in
    let param k =
      let x = code.(i + k) in
      match mode k with
      | 0 (* position  *) -> code.(x)
      | 1 (* immediate *) -> x
      | _ -> assert false in
    match instr with
    | 99 (* halt *)->
        ()
    | 1 | 2 as o (* arith *) ->
        let x = param 1 and y = param 2 and z = code.(i + 3) in
        code.(z) <- if o = 1 then x+y else x*y;
       exec (i + 4)
    | 3 (* input *) ->
        let x = code.(i + 1) in
        code.(x) <- input ();
        exec (i + 2)
    | 4 (* output *) ->
        let x = param 1 in
        output x;
        exec (i + 2)
    | 5 (* jump-if-true *) ->
        let x = param 1 in
        if x <> 0 then exec (param 2) else exec (i + 3)
    | 6 (* jump-if-false *) ->
        let x = param 1 in
        if x = 0 then exec (param 2) else exec (i + 3)
    | 7 (* less than *) ->
        let x = param 1 and y = param 2 and z = code.(i + 3) in
        code.(z) <- iverson (x < y);
        exec (i + 4)
    | 8 (* equals *) ->
        let x = param 1 and y = param 2 and z = code.(i + 3) in
        code.(z) <- iverson (x = y);
        exec (i + 4)
    | _ ->
        assert false
  in
  exec 0
