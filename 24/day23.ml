
open Format
open Lib

let computers = H.create (1 lsl 16)
let connected = H.create (1 lsl 16)

let add s =
  let x, y = split2 ~sep:'-' s in
  H.replace computers x ();
  H.replace computers y ();
  H.replace connected (x, y) ();
  H.replace connected (y, x) ()

let () = iter_lines stdin add
let () = printf "%d computers@." (H.length computers)

let () =
  let ans = H.create 16 in
  let visit x _ =
    if String.starts_with ~prefix:"t" x then
      H.iter (fun y _ ->
      H.iter (fun z _ ->
      if H.mem connected (x, y) && H.mem connected (y, z)
      && H.mem connected (z, x) then (
        let s = List.sort Stdlib.compare [x;y;z] in
        H.replace ans s ()
      )
      ) computers) computers
  in
  H.iter visit computers;
  printf "%d@." (H.length ans)


