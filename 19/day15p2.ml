
open Format
open Lib
open Machine

let code = read stdin

let north = 1 and south = 2 and west = 3 and east = 4
let all_dirs = [Grid.N; Grid.E; Grid.S; Grid.W]
let send = function
  | Grid.N -> north
  | Grid.E -> east
  | Grid.S -> south
  | Grid.W -> west
  | _ -> assert false

let send d =
  let v = send d in
  printf "send %d (%s)@." v
    (if v = north then "north" else if v = south then "south" else
     if v = east then "east" else "west");
  v

let back = function
  | Grid.N -> Grid.S
  | Grid.E -> Grid.W
  | Grid.S -> Grid.N
  | Grid.W -> Grid.E
  | _ -> assert false
let wall = 0 and empty = 1 and oxygen = 2

(* Principle: we run a BFS *)

let start = (0, 0)
let pos = ref start

let path = H.create 16 (* position -> direction that reached it *)
let dist = H.create 16 (* position -> distance from start *)
let is_wall p = H.find_opt dist p = Some (-1)

let queue = Queue.create ()
let () = Queue.add (0,0) queue; H.add dist start 0

(* We always know how to go back to the start point (`backtrack`)
   and to go to any point which is already known (`goto`). *)

let rec backtrack p = (* from p to start *)
  if p = start then [] else
  let d = back (H.find path p) in d :: backtrack (Grid.move d p)
let rec goto p = (* from start to p *)
  if p = start then [] else
  let d = H.find path p in let q = Grid.move (back d) p in goto q @ [d]

type todo =
  | Move of Grid.direction
  | Peek of Grid.direction

let last = ref None       (* last action we did *)
let todo = Stack.create ()        (* to do next *)
let out = ref 1 (* last output from the machine *)

let print () = (* curiosity *)
  let xmin = ref max_int and xmax = ref min_int in
  let ymin = ref max_int and ymax = ref min_int in
  H.iter (fun (x, y) _ -> xmin := min !xmin x;
                          xmax := max !xmax x;
                          ymin := min !ymin y;
                          ymax := max !ymax y) dist;
  let g = Grid.make (!ymax - !ymin + 1) (!xmax - !xmin + 1) ' ' in
  H.iter (fun (x, y) n -> Grid.set g (y - !ymin, x - !xmin)
    (if n = -2 then '.' else '#')) dist;
  printf "%a@." Grid.print_chars g;
  exit 0

let oxygen = ref (max_int, max_int)

let fill_with_oxygen () =
  let mark p = H.replace dist p (-2) in
  let rec fill time pl =
    List.iter mark pl;
    let add p acc d =
      let q = Grid.move d p in
      match H.find_opt dist q with
      | None -> assert false
      | Some n when n < 0 -> acc
      | Some _ -> q :: acc in
    let add acc p = List.fold_left (add p) acc all_dirs in
    let pl = List.fold_left add [] pl in
    if pl = [] then (printf "count = %d@." time; print ());
    fill (time + 1) pl
  in
  fill 0 [!oxygen]

let rec input () =
  printf "  out %d (%s)@." !out (if !out = wall then "wall" else "empty");
  match !last with
  | None ->
      next ()
  | Some (Move d) ->
      assert (!out <> wall);
      pos := Grid.move d !pos;
      next ()
  | Some (Peek d) ->
      if !out = wall then (
        H.add dist (Grid.move d !pos) (-1);
        next ()
      ) else (
        let di = H.find dist !pos in
        pos := Grid.move d !pos;
        assert (not (H.mem dist !pos));
        H.add path !pos d;
        H.add dist !pos (di+1);
        if !out = 2 then oxygen := !pos;
        Queue.add !pos queue;
        Stack.push (Move (back d)) todo;
        next ()
      )
and next () =
  if Stack.is_empty todo then (
    if Queue.is_empty queue then fill_with_oxygen ();
    let p = Queue.pop queue in
    List.iter (fun d ->
        let q = Grid.move d p in
        if not (H.mem dist q) then Stack.push (Peek d) todo;
      ) all_dirs;
    if p <> !pos then (
      let moves = backtrack !pos @ goto p in
      List.iter (fun d -> Stack.push (Move d) todo) (List.rev moves)
    )
  );
  let Move d | Peek d as a = Stack.pop todo in
  last := Some a;
  send d

let () = exec code input (fun v -> out := v)
