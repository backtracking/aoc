
open Lib
open Format

let g = H.create 16
let () = H.add g "out" []
let () =
  let add s =
    let src, s = split2 ~sep:':' s in
    H.add g src (split_strings s) in
  iter_lines stdin add

let count target = memo (fun count v ->
  if v = target then 1 else
  List.fold_left (fun acc w -> acc + count w) 0 (H.find g v)
)

let count src dst = count dst src

let svr_fft = count "svr" "fft"
let svr_dac = count "svr" "dac"
let fft_dac = count "fft" "dac"
let dac_fft = count "dac" "fft"
let dac_out = count "dac" "out"
let fft_out = count "fft" "out"

let () = printf "svr -> fft: %d paths@." svr_fft
let () = printf "svr -> dac: %d paths@." svr_dac
let () = printf "fft -> dac: %d paths@." fft_dac
let () = printf "dac -> fft: %d paths@." dac_fft
let () = printf "dac -> out: %d paths@." dac_out
let () = printf "fft -> out: %d paths@." fft_out

let () = printf "total %d@."
  (svr_fft * fft_dac * dac_out +
   svr_dac * dac_fft * fft_out)

