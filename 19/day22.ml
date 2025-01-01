
open Format
open Lib

let size = int_of_string Sys.argv.(1)

let deal_into_new inp out =
  for i = 0 to size - 1 do out.(i) <- inp.(size - 1 - i) done

let cut s inp out =
  let n = Scanf.sscanf s "cut %d" (fun n -> n) in
  if n >= 0 then (
    A.blit inp 0 out (size - n) n;
    A.blit inp n out 0          (size - n)
  ) else (
    let n = -n in
    A.blit inp (size - n) out 0 n;
    A.blit inp 0          out n (size - n)
  )

let deal_with_increment s inp out =
  let n = Scanf.sscanf s "deal with increment %d" (fun n -> n) in
  let next = ref 0 in
  for i = 0 to size - 1 do
    out.(!next) <- inp.(i);
    next := (!next + n) mod size
  done

let rec loop inp out =
  match input_line stdin with
  | s when String.starts_with ~prefix:"cut" s -> cut s inp out; loop out inp
  | s when String.starts_with ~prefix:"deal with increment" s ->
      deal_with_increment s inp out; loop out inp
  | "deal into new stack" -> deal_into_new inp out; loop out inp
  | _ -> assert false
  | exception End_of_file -> inp

let () =
  let res = loop (A.init size (fun i -> i)) (A.make size 0) in
  if size = 10 then (
    printf "Result: ";
    for i = 0 to size - 1 do printf " %d" res.(i); done; printf "@."
  ) else
    for i = 0 to size - 1 do
      if res.(i) = 2019 then printf "2019 at position %d@." i
    done



