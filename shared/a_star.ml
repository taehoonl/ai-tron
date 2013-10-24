open Definitions
open Constants
open Util

module MakePQueue (Ord : Set.OrderedType) =
struct

  type elt = Ord.t
  type t = elt option array ref * int ref

  let swap arr i1 i2 = let tmp = arr.(i1) in
    arr.(i1) <- arr.(i2);
    arr.(i2) <- tmp

  let cmp x y = match x, y with
  | Some a, Some b -> Ord.compare a b
  | _,_ -> failwith "compared none"

  let create () = (ref (Array.make 16 None), ref 0)

  let parent i = (i-1)/2

  let l_child i = 2*i + 1
  let r_child i = 2*i + 2

  let insert (h_ref,i) elt =
    let elt = Some elt in
    let h = !h_ref in
    h.(!i) <- elt;
    let rec fix_heap index =
      if index = 0 then () else
      let p_index = parent index in
      (* if the parent is larger than the child, swap*)
      if cmp h.(p_index) h.(index) > 0 then 
        (swap h p_index index;
        fix_heap p_index)
      else fix_heap p_index in
    fix_heap !i;
    incr i;
    if !i >= Array.length h then
      let to_append = Array.make (Array.length h) None in
      h_ref := Array.append h to_append

  let extract_min (h_ref, i) =
    decr i;
    let h = !h_ref in
    if h.(0) = None then None else
    let to_ret = h.(0) in
    h.(0) <- h.(!i);
    h.(!i) <- None;
    let rec fix_heap index =
      let l = l_child index in let r = r_child index in
      if l >= !i && r >= !i then () else
      if h.(r) = None && h.(l) = None then () else
      let smaller_child = if h.(r) = None then l
        else if cmp h.(r) h.(l) < 0 then r else l in
      if h.(smaller_child) < h.(index) then
        (swap h smaller_child index; fix_heap smaller_child)
      else () in
    fix_heap 0;
    to_ret
end

let manhattan src dest =
  let (x1, y1) = src in
  let (x2, y2) = dest in
  abs_float (float_of_int (x2 - x1)) +. abs_float (float_of_int (y2 - y1))

let euclidian src dest = 
  let (x1, y1) = src in
  let (x2, y2) = dest in
  sqrt (float_of_int (((x1-x2) * (x1-x2)) + ((y1-y2) * (y1-y2))))
 
module MyOrd =
struct 
  type t = ((int*int)*float)
  let compare (p1, f1) (p2, f2) = compare f1 f2
end

module PQ = MakePQueue(MyOrd)

let a_star ((r1,c1) as source) ((r2,c2) as dest) tail_list heauristic =
  (* data structures *)
  let obstacle = Hashtbl.create (List.length tail_list) in
  let history = Hashtbl.create 42 in
  let dist = Hashtbl.create (cNUM_COLUMNS) in
  let score = Hashtbl.create (cNUM_COLUMNS) in
  let visited = Hashtbl.create cNUM_COLUMNS in (* already evaluated *)
  let unexplored = Hashtbl.create cNUM_COLUMNS in
  let unvisited = PQ.create () in (* to be evaluated *)

  (* helper fun *)
  let update_tbl table k v =
    if Hashtbl.mem table k then 
      Hashtbl.replace table k v 
    else Hashtbl.add table k v
  in

  let get_dist table k = try Hashtbl.find table k with _ -> max_int in

  let rec trace_back node acc =
    if Hashtbl.mem history node 
    then trace_back (Hashtbl.find history node) (node::acc)
    else (node::acc)
  in

  (* initialize *)
  List.iter (fun x -> Hashtbl.add obstacle x true) tail_list; (* obstacles *)
  Hashtbl.add dist source 0; (* source dist *)
  Hashtbl.add score source (heauristic source dest); (* source score *)
  Hashtbl.add unexplored source true;
  PQ.insert unvisited (source, (Hashtbl.find score source)); (* start point *)

  let rec calculate_path () =
    match PQ.extract_min unvisited with
    | None -> [] (* path not found *)
    | Some ((c,r),s) -> (* explore *)
      (if (c,r) = dest then (trace_back dest [])
       else 
	  (Hashtbl.remove unexplored (c,r);
	   update_tbl visited (c,r) true;
	   (* right,left,down,up *)
	   let lst = [(c+1,r);(c-1,r);(c,r+1);(c,r-1)] in 
	   let neighbors = List.filter (*check boundary + obstacles*)
	     (fun (x,y)->(x>=0 && y>=0 && y< cNUM_ROWS && x< cNUM_COLUMNS && 
			  not(Hashtbl.mem obstacle (x,y)))) lst in
	   List.iter 
	     (fun (x,y) -> 
	       (let nd = (Hashtbl.find dist (c,r)) + 1 in
		if ((Hashtbl.mem visited (x,y))&&(nd>=get_dist dist (x,y)))
		then () (* next iter *)
		else
		  (if (not(Hashtbl.mem unexplored (x,y)))
		      ||(nd<get_dist dist (x,y))
		   then 
		      ((if nd < get_dist dist (x,y) 
			then (update_tbl history (x,y) (c,r); 
			      update_tbl dist (x,y) nd) else ());
		       update_tbl score (x,y) ((float_of_int(nd)) +. 
						  (heauristic (x,y) dest));
		       if not(Hashtbl.mem unexplored (x,y))
		       then 
			 PQ.insert unvisited ((x,y),(Hashtbl.find score (x,y)))
		       else () )
		   else () ) ) ) neighbors;
	   calculate_path ()
	  )
      )
  in
  calculate_path ()

(*a_star (10,10) (21,21) [(20,5);(20,6);(20,7);(20,8);(20,9);(20,10);(20,11)
;(20,12);(20,13);(20,14);(20,15);(20,16);(20,17);(20,18);(20,19);(20,20);
(11,20);(12,20);(13,20);(14,20);(15,20);(16,20);(17,20);(18,20);(19,20)] euclidian

let src = (2,3)
let dst = (2,1)
let tails = [(2,2); (1,2); (0,2); (3,2)]

let () =
  let path = a_star src dst tails manhattan in
  print_endline (str_tup_lst path)*)
