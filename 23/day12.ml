
open Format
open Lib

let solve = memo (fun solve (len, cl, nl) -> match cl, nl with
  | _, n :: _ when len > n -> 0
  | [], [] -> if len > 0 then 0 else 1
  | [], [n] -> if len = n then 1 else 0
  | [], _ :: _ :: _ -> 0
  | '#' :: cl, nl -> solve (len+1, cl, nl)
  | '.' :: _, [] when len > 0 -> 0
  | '.' :: cl, n :: nl when len > 0 ->
      if len <> n then 0 else solve (0, cl, nl)
  | '.' :: cl, nl -> solve (0, cl, nl)
  | '?' :: cl, nl -> solve (len, '#' :: cl, nl) + solve (len, '.' :: cl, nl)
  | _ :: _, _ -> assert false
)

let solve1 s =
  let s, nl = split2 s in
  solve (0, list_of_string s, split_ints ~sep:',' nl)

let solve2 s =
  let s,nl = split2 s in
  let s = String.concat "?" [s;s;s;s;s] in
  let nl = split_ints ~sep:',' nl in
  solve (0, list_of_string s, nl@nl@nl@nl@nl)

let () = assert (solve1 "???.### 1,1,3" = 1)
let () = assert (solve1 ".??..??...?##. 1,1,3" = 4)
let () = assert (solve1 "?###???????? 3,2,1" = 10)

let () = assert (solve2 ".??..??...?##. 1,1,3" = 16384)

let sum = fold_lines stdin (fun s sum -> solve2 s + sum) 0
let () = printf "sum = %d@." sum
