
open Format
open Scanf

let sum = ref 0

let scan s = match String.split_on_char ':' s with
  | [gid; s] ->
      let id = sscanf gid "Game %d" (fun id -> id) in
      printf "s = %S@." s;
      let check draw =
        let r = ref 0 and g = ref 0 and b = ref 0 in
        let add s f x =
          sscanf s f (fun n -> x := n) in
        let add s =
          try add s " %d red" r  with _ ->
          try add s " %d green" g with _ ->
          try add s " %d blue" b with _ -> () in
        List.iter add (String.split_on_char ',' draw);
        !r <= 12 && !g <= 13 && !b <= 14
      in
      if List.for_all check (String.split_on_char ';' s) then
        sum := !sum + id
  | _ -> assert false


let () =
  while true do match read_line () with
  | l -> scan l
  | exception End_of_file -> printf "sum = %d@." !sum; exit 0
  done
