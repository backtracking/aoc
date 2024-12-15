
open Format
open Lib
open Grid

let g =
  let rec scan rows = match input_line stdin with
    | "" ->
        let get s i = match s.[i/2] with
          | '#' | '.' as c -> c
          | '@' -> if i mod 2 = 0 then '@' else '.'
          | 'O' -> if i mod 2 = 0 then '[' else ']'
          | _ -> assert false
        in
        let row s = Array.init (2 * String.length s) (get s) in
        Array.map row (Array.of_list (List.rev rows))
    | s -> scan (s :: rows)
  in
  scan []

let () = printf "%a@." print_chars g
let () = printf "%dx%d@." (height g) (width g)

let robot = ref (find (fun _ c -> c = '@') g)
let () = set g !robot '.'

let direction = function
  | '<' -> W
  | '>' -> E
  | '^' -> N
  | 'v' -> S
  | _ -> assert false

let is_box p = let c = get g p in c = '[' || c = ']'
let opposite = function W -> E | E -> W | _ -> assert false

module S = Set.Make(struct type t = position let compare = Stdlib.compare end)

let move d =
  let p = move d !robot in
  match get g p with
  | '#' -> ()
  | '.' -> robot := p
  | '[' | ']' when d = W || d = E -> (* horizontal *)
        let q = ref p in while is_box !q do q := move d !q done;
        if get g !q = '.' then (
          let o = opposite d in
          while is_box (move o !q) do
            let r = move o !q in set g !q (get g r); q := r done;
          robot := p; set g p '.';
        )
  | '[' | ']' -> (* vertical *)
      let rec push p =
        let p1, p2 = if get g p = '[' then p, move E p else move W p, p in
        let q1 = move d p1 and q2 = move d p2 in
        if get g q1 = '.' && get g q2 = '.' then Some (S.singleton p1) else
        if get g q1 = '#' || get g q2 = '#' then None else
        if get g q1 = '.' then match push q2 with
                               | Some bs -> Some (S.add p1 bs)
                               | None -> None else
        if get g q2 = '.' then match push q1 with
                               | Some bs -> Some (S.add p1 bs)
                               | None -> None else
        match push q1, push q2 with
        | Some s1, Some s2 -> Some (S.add p1 (S.union s1 s2))
        | _ -> None
      in
      (match push p with
       | Some boxes ->
        S.iter (fun q -> set g q '.'; set g (move E q) '.') boxes;
        S.iter (fun q -> let q = move d q in set g q '['; set g (move E q) ']')
          boxes;
        robot := p
       | None -> ())
  | _ -> assert false

let () =
  try while true do match input_char stdin with
  | '\n' -> ()
  | '<' | '>' | '^' | 'v' as c ->
      move (direction c)
  | _ -> assert false
  done with End_of_file ->
  printf "%a@." print_chars g;
  let ans =
    fold (fun (i,j) c acc -> if c = '[' then acc + 100*i+j else acc) g 0 in
  printf "%d@." ans


