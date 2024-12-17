
open Format
open Lib

let register () =
  Scanf.sscanf (input_line stdin) "Register %c: %d" (fun _ n -> n)
let a0 = register ()
let b0 = register ()
let c0 = register ()

let () = ignore (input_line stdin)
let orig, code =
  let s = input_line stdin in
  assert (String.starts_with ~prefix:"Program: " s);
  let s = String.sub s 9 (String.length s - 9) in
  s, Array.of_list (split_ints ~sep:',' s)
let size = Array.length code

let exec a b c code =
  let a = ref a and b = ref b and c = ref c in
  let combo i =
    let v = code.(i) in
    if v <= 3 then v else match v with
    | 4 -> !a
    | 5 -> !b
    | 6 -> !c
    | _ -> assert false in
  let out = Buffer.create 16 in
  let comma = ref false in
  let output n =
    if !comma then Buffer.add_char out ',' else comma := true;
    Buffer.add_char out (char_of_int n) in
  let rec exec pc =
  printf "  A=%a B=%a C=%a@." print_binary !a print_binary !b print_binary !c;
  if pc < size then match code.(pc) with
  | 0 (* adv *) -> a := !a / (pow 2 (combo (pc + 1))); exec (pc + 2)
  | 1 (* bxl *) -> b := !b lxor code.(pc + 1); exec (pc + 2)
  | 2 (* bst *) -> b := combo (pc + 1) land 0b111; exec (pc + 2)
  | 3 (* jnz *) -> exec (if !a = 0 then pc + 2 else code.(pc + 1))
  | 4 (* bxc *) -> b := !b lxor !c; exec (pc + 2)
  | 5 (* out *) -> output (combo (pc + 1) land 0b111); exec (pc + 2)
  | 6 (* bdv *) -> b := !a / (pow 2 (combo (pc + 1))); exec (pc + 2)
  | 7 (* cdv *) -> c := !a / (pow 2 (combo (pc + 1))); exec (pc + 2)
  | _ -> assert false
  in
  exec 0;
  Buffer.contents out

let () =
  printf "%s@." (exec a0 b0 c0 (Array.copy code))


let () =
  let start = ref (int_of_string Sys.argv.(1)) in
  printf "start = %d (%a)@." !start print_binary !start;
  let s = exec !start b0 c0 (Array.copy code) in
  printf "  => %s@." s;
  if s = orig then printf "A = %d@." !start

(*
Register A: ???
Register B: 0
Register C: 0
Program: 2,4,1,5,7,5,1,6,0,3,4,6,5,5,3,0

 2,4, B := A mod 8
 1,5, B := B xor 101
 7,5, C := A / 2^B
 1,6, B := B xor 110
 0,3, A := A / 8
 4,6, B := B xor C
 5,5, out (B mod 8)
 3,0  jnz A 0

Analysis:
- the output depends on the LS 10 bits
- A is shifted 3 bits to the right at each iteration, until 0
- to get the final output 0, we need 011 for the MS bits of (initial) A
  (because 011 XOR 101 XOR 110 = 000 and thus upper bits of A are
   output without being negated)
- from this, we find out the next 3 bits to get the output 3, and so on
  reading the expected output from right to left

We end up with the following bits

  011_111_001_000_001_110_101_001_001_000_010_110_100_110_011_010

that is

  A = 136904920099226
*)
