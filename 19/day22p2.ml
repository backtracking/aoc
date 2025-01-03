
open Format
open Lib

let size   = 119_315_717_514_047
let repeat = 101_741_582_076_661

type op =
  | DealIntoNew
  | Cut of int
  | DealWithIncrement of int

let parse s =
  match s with
  | s when String.starts_with ~prefix:"cut" s ->
      Scanf.sscanf s "cut %d" (fun n -> Cut n)
  | s when String.starts_with ~prefix:"deal with increment" s ->
      Scanf.sscanf s "deal with increment %d" (fun n -> DealWithIncrement n)
  | "deal into new stack" -> DealIntoNew
  | _ -> assert false

let ops = map_lines stdin parse
let () = printf "%d operations@." (List.length ops)
let revops = List.rev ops

module M = Modular(struct let m = size end)

(* the inverse of an operation is an affine transformation i -> ai + b *)

let inverse = function
  | DealIntoNew ->
      M.of_int (-1), size - 1
  | Cut n ->
      M.one, M.of_int n
  | DealWithIncrement n ->
      M.div M.one (M.of_int n), M.zero

(* the inverse of all operations is the composition *)

let a, b =
  let compose (a, b) op =
    let c, d = inverse op in
    M.(c ** a, c ** b ++ d) in
  List.fold_left compose (M.one, M.zero) revops

module Ma = Matrix(M)
let m = [| [| a; b |];
           [| 0; 1 |] |]

let apply i =
  (Ma.power_apply m repeat [| i; 1 |]).(0)

let target = 2020
let () = printf "%d@." (apply target)
