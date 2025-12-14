
open Lib
open Format
open Combine
open Combine.Tiling
module P = Pattern

type tile = Tile.t
type problem = int * int * int array

let tiles, problems =
  let tl = ref [] in
  let pl = ref [] in
  try while true do
    let s = input_line stdin in
    try Scanf.sscanf s "%d:" @@ fun i ->
       printf "%d:@." i;
       let rec parse_tile acc =
         let s = input_line stdin in
         if s = "" then (
           let g = Array.of_list acc in
           let sz = Grid.fold (fun _ c sz -> iverson (c) + sz) g 0 in
           let p = P.create g in
           let t = Tile.(create ~name:(string_of_int i) ~m:Mone ~s:Sall p) in
           printf "%a@.------@." Tile.print t;
           tl := (sz,t) :: !tl
           ) else
         let acc = Array.init (String.length s) (fun i -> s.[i] = '#') :: acc in
         parse_tile acc
       in
       parse_tile []
    with _ -> Scanf.sscanf s "%dx%d:" @@ fun h w ->
      let _, s = split2 ~sep:':' s in
      let il = split_ints ~sep:' ' s in
      printf "%dx%d: @[%a@]@." h w (pp_print_list ~pp_sep:comma pp_print_int) il;
      pl := (h, w, il) :: !pl
   done with End_of_file -> Array.of_list (List.rev !tl), !pl

let () = printf "%d tiles, %d problems@." (Array.length tiles)
           (List.length problems)

let total = ref 0

let solve (h, w, nums) =
  printf "solve %dx%d @[%a@]@." h w (pp_print_list ~pp_sep:comma pp_print_int)
    nums;
  let open Problem in
  let tl = ref [] in
  let sz = ref 0 in
  let nt = ref 0 in
  let add i n =
    printf "%d x tile %d@." n i;
    nt += n;
    sz += fst tiles.(i) * n;
    for _ = 1 to n do tl := snd tiles.(i) :: !tl done in
  Array.iteri add (Array.of_list nums);
  if !sz > h*w then printf "too many tiles!@." else
  if (3*(h/3))*(3*(w/3)) >= 9 * !nt then (printf "obvious!@."; incr total) else
  let g = Pattern.create (Array.make_matrix w h true) in
  let p = create g !tl in
  (* printf "problem @[%a@]@." print p; *)
  let emc = ToEMC.make ~partially:true p in
  printf "@[%a@]@." ToEMC.print_emc_size emc;
  let columns = emc.columns in
  let ToEMC.{ primary = primary; emc = m; tiles = _decode_tbl; _ } = emc in
  let dlx = Emc.D.create_sparse ~columns ~primary m in
  try
    let s = Emc.D.find_solution dlx in
    printf "solution!@.";
    printf "%a@." (fun fmt -> ToEMC.print_solution_ascii fmt p emc) s;
    incr total
  with Not_found -> printf "no solution@."

let () = List.iter solve problems; printf "%d@." !total

