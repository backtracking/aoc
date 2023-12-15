
open Format
open Lib

let rec hash s acc i =
  if i = String.length s then acc else
  hash s (((acc + Char.code s.[i]) * 17) mod 256) (i+1)
let hash s = hash s 0 0

let boxes = Array.make 256 []

let remove i lab =
  boxes.(i) <- List.filter (fun (l,_) -> l<>lab) boxes.(i)

let rec add lab fl = function
  | [] -> raise Not_found
  | (l, _) :: ll when l = lab -> (l, fl) :: ll
  | lfl :: ll -> lfl :: add lab fl ll

let add i lab fl =
  boxes.(i) <- try add lab fl boxes.(i) with Not_found -> (lab, fl) :: boxes.(i)

let eval s =
  let n = String.length s in
  if s.[n - 1] = '-' then
    let lab = String.sub s 0 (n - 1) in
    remove (hash lab) lab
  else
    let lab, fl = split2 ~sep:'=' s in
    add (hash lab) lab (int_of_string fl)

let s = input_line stdin
let l = split_strings ~sep:',' s
let () = List.iter eval l

let () =
  let rec print fmt = function
      | [] -> ()
      | (lab, fl) :: ll -> printf " [%s %d]%a" lab fl print ll in
  let print i b = printf "box %d:%a@." i print b in
  Array.iteri (fun i b -> if b <> [] then print i b) boxes

let rec power i = function
  | [] -> 0
  | (_,fl) :: ll -> i * fl + power (i-1) ll

let sum = ref 0
let () = for i = 0 to 255 do
           let b = boxes.(i) in sum += (i+1) * power (List.length b) b done
let () = printf "sum = %d@." !sum
