
open Format
open Lib
module H = Hashtbl

type pulse = L | H
let print_pulse fmt p = fprintf fmt "%c" (if p = L then 'L' else 'H')

type kind = BC | FF of bool ref | CJ of (string, pulse) H.t
type modul = kind * string list

let modules : (string, modul) H.t = H.create 16

let () =
  let destination s =
    let n = String.length s in
    let i = String.index s '>' in
    let l = split_strings ~sep:',' (String.sub s (i+1) (n-i-1)) in
    List.map String.trim l in
  let add s =
    if String.starts_with ~prefix:"broadcaster -> " s then
      H.add modules "broadcaster" (BC, destination s)
    else (
      let name = let i = String.index s ' ' in String.sub s 1 (i - 1) in
      let k = if s.[0] = '%' then FF (ref false)
              else (assert (s.[0] = '&'); CJ (H.create 16)) in
      H.add modules name (k, destination s)
    )
  in
  iter_lines stdin add

(* fill CJ tables *)
let () =
  let f org dst =
    match H.find modules dst with
    | CJ t, _ -> H.replace t org L
    | exception Not_found -> ()
    | _ -> () in
  let fill org (_, dl) = List.iter (f org) dl in
  H.iter fill modules

let _, bcl = H.find modules "broadcaster"

let () =
  let actions = Queue.create () in
  let nbL = ref 0 and nbH = ref 0 in
  let send m1 p m2 =
    incr (if p = L then nbL else nbH);
    Queue.add (m1, p, m2) actions in
  let sends m1 p dl = List.iter (send m1 p) dl in
  let all_high t = H.fold (fun _ p b -> p = H && b) t true in
  let nbpress = ref 0 in
  (* part I *) for n = 1 to 1000 do (*printf "@.iteration %d@." n; *)
  (* part II *) (* while true do *)
  sends "broadcaster" L bcl; incr nbL; incr nbpress;
  while not (Queue.is_empty actions) do
    let m1, p, m2 = Queue.pop actions in
(* (* test for part II *)
    if p=H && m2 = "gh" then (
      printf "gh receives H after %d button press!@." !nbpress; exit 0);
*)
    match H.find modules m2 with
    | FF b, dl -> if p = L then (b := not !b; sends m2 (if !b then H else L) dl)
    | CJ t, dl -> H.replace t m1 p; sends m2 (if all_high t then L else H) dl
    | BC, _ -> assert false
    | exception Not_found -> ()
  done
  done;
  printf "%dL x %dH = %d@." !nbL !nbH (!nbL * !nbH)

(* part 2 :

there are 4 independent components, all connected to &gh

              ---> fb ... qx -->
  broadcaster ---> xk ... cd --> &gh -> rx
              ---> gr ... zf -->
              ---> vj ... rk --->

using the code above (replace the for loop with the while loop and
uncomment the test), we compute the number of button press before H is
send to gh for each bloc:

  bloc 0 : 4057
  bloc 1 : 3793
  bloc 2 : 3947
  bloc 3 : 3733

these are four prime numbers, and the answer is therefore the product

  226732077152351
*)
