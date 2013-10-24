open Team
open Definitions
open Constants
open Util
open A_star

let _ = Random.self_init ()

let get_random_index lst =
  Random.int (List.length lst)
  
let get_random_rider lst = 
  List.nth lst (get_random_index lst)
	
let rand_orientation _ =
  match get_random_num 4 with
  | 0 -> North
  | 1 -> South
  | 2 -> East
  | _ -> West

let moveTo rider me des: orientation=
  let (c1,r1) = me in 
  let (c2,r2)=des in
  if c1=c2 && (r1-r2)=1 then (North)
  else if c1=c2 && (r2-r1)=1 then (South)
  else if (c1-c2)=1 && r1=r2 then (West)
  else if (c2-c1)=1 && r1=r2 then (East)
  else rider.orientation
    
let bot c =
  while true do
    let (opp_riders, opp_items) = match get_status (GameStatus) with 
      | GameData ((r,i1),(b,i2),_) -> if c=Red then (b,i2) else (r,i1)
      | _ -> failwith "oplist status fail" in	
    let (rider_list,item_list) = match get_status (TeamStatus c) with 
      | TeamData a -> a 
      | _ -> failwith "Teamdata status fail" in	
    let rider_tiles = 
      List.map (fun x-> x.tile) (List.append opp_riders rider_list) in
		
    let (sh_team,inv_team) = List.partition (fun (i,n) -> i=Shield) item_list in
    let (sh_num,inv_num) = (snd (List.hd sh_team),snd (List.hd inv_team)) in
    let sh_num = ref sh_num in
    let inv_num = ref inv_num in

    let (sh_opp_team,inv_opp_team) = 
      List.partition (fun (i,n) -> i=Shield) opp_items in
    let (sh_opp_num,inv_opp_num) = 
      (snd (List.hd sh_opp_team),snd (List.hd inv_opp_team)) in
	
    let field_data =
      match get_status (FieldItemsStatus) with FieldItemsData a -> a
      | _ -> failwith "Fielditem status fail" in

    let tail_data = match get_status (TailStatus) with TailData a -> a
      | _ -> failwith "Tail status fail" in

    (* shield riders if necessary && possible *)
    List.iter (fun x -> 
      if (!sh_num <> 0) && not(List.mem Shielded x.modifiers)
      then ( let _ = send_action (UseItem(x.id, Shield)) in 
	     sh_num := !sh_num - 1 )
      else ()) rider_list;

    (*check North South West East and
      see if it is not blocked by tail or rider or out of bound*)
    let check_around rider ori =
      let (c,r) = rider.tile in (*current rider position*)
      (*left/right/down/up*)
      let diamond (col,row)=[(col-1,row);(col+1,row);(col,row+1);(col,row-1)] in
      let around = diamond (c,r) in
      let b_tile = 
	List.filter 
	  (fun (x,y) -> x>=0 && y>=0 && x <cNUM_COLUMNS && y <cNUM_ROWS) 
	  around in
      let bound_tile = (*check boundary conditions*)
	List.map (fun x -> 
	  if x=(c-1,r) then West  else if x=(c+1,r) then East 
	  else if x=(c,r+1) then South else North) b_tile 
      in

      let o_tile = 
	List.filter 
	  (fun x -> not(List.mem x tail_data) && not(List.mem x rider_tiles)) 
	  b_tile in
      let open_tile = (*check if tiles are empty*)
	List.map (fun x -> 
	  if x=(c-1,r) then West  else if x=(c+1,r) then East 
	  else if x=(c,r+1) then South else North) o_tile 
      in

      let next_tile = if ori = North then (c,r-1) 
	else if ori = South then (c,r+1) 
	else if ori = West then (c-1,r) else (c+1,r) in
      let next_open_tile = (*open tiles of next move*)
	let b_tile =  
	  List.filter (fun (c,r) -> c >=0 && r >=0 && c <cNUM_COLUMNS 
	    && r <cNUM_ROWS) (diamond next_tile) in
	List.filter (fun x -> not(List.mem x tail_data) && 
	  not(List.mem x rider_tiles)) b_tile
      in
      let orient =
	if List.length open_tile = 0 then (*no open tile exists*)
	  ((if !inv_num > 0 && not(List.mem Invincible rider.modifiers)
	    then (let _ = send_action (UseItem(rider.id, Invincibility)) in 
		  inv_num := !inv_num - 1)
	    else ());
	   if List.mem Invincible rider.modifiers && List.mem ori bound_tile 
	   then ori
	   else List.nth bound_tile (get_random_num (List.length bound_tile)))
	else if List.length next_open_tile < 1 then (*dead end*)
	  (let choice = List.filter (fun x -> x <> ori) open_tile in
	   if choice = [] then ori 
	   else List.nth choice (get_random_num (List.length choice)))
	else (*open tile exists*)
	  (if not(List.mem ori open_tile) then 
	      List.nth open_tile (get_random_num (List.length open_tile))
	   else ori)
      in

      if List.mem orient bound_tile || List.mem orient open_tile then orient
      else List.nth bound_tile (get_random_num (List.length bound_tile))
    in

    (*check for future collisions*)
    let check_diamond rider = 
      let id = rider.id in
      let (c,r) = rider.tile in

      let direction coor dir =
	if coor = (c,r-2) || coor = (c -1, r -1) then 
	  (if dir then North else West)
	else if coor = (c +1, r -1) || coor = (c+2, r) then 
	  (if dir then East else South)
	else if coor = (c-2, r) || coor = (c -1, r +1) then 
	  (if dir then West else North)
	else (if dir then South else East)
      in

      let pos = [(c,r-2);(c-1,r-1);(c+1,r-1);(c-2,r);
		 (c+2,r);(c,r+2);(c-1,r +1);(c +1, r +1)] in
      let diamond = List.filter (fun (col,row) -> col >= 0 && row >= 0 
	&& col < cNUM_COLUMNS && row < cNUM_ROWS) pos in
      let enemies = List.filter (fun e -> List.mem e.tile diamond) opp_riders in
      let team_riders = List.filter 
	(fun r -> List.mem r.tile diamond) rider_list in

      if List.length enemies > 0 then
	(let inv_enemies = List.filter 
	   (fun x -> (List.mem Invincible x.modifiers)) enemies in
	 let shi_enemies = List.filter 
	   (fun x -> List.mem Shielded x.modifiers) enemies in
	 let en_score = 
	   if (List.length inv_enemies > 0) || inv_opp_num > 0 then 3 
	   else if (List.length shi_enemies > 0) || sh_opp_num > 0 then 2
	   else 1 in
	 let my_score = 
	   if List.mem Invincible rider.modifiers || !inv_num > 0 then 3 
	   else if (List.mem Shielded rider.modifiers) || !sh_num > 0 then 2
	   else 1 in
	 if my_score > en_score then (* attack enemy for 1 step *)
	   ((if (my_score = 3 && not(List.mem Invincible rider.modifiers) 
		&& !inv_num > 0)
	     then (
	       let _ = send_action (UseItem(id, Invincibility)) in
	       inv_num := !inv_num - 1)
	     else ());
	    let enemy = List.hd enemies in
	    direction enemy.tile true)
	 else (* avoid enemy *)
	   (let enemy = List.hd enemies in
	    direction enemy.tile false))
      else if List.length team_riders > 0 then (*avoid our team*)
	(let rider = List.hd team_riders in
	 direction rider.tile false) 
      else (
	(*get item*)
	if List.length field_data > 0 then (*only if there are enough items*)	  
	  let field_range = 
	    List.filter (fun (_,tl)-> 
	      (manhattan tl rider.tile) < (float_of_int cNUM_COLUMNS)) 
	      field_data in
	  let field_min = (List.sort (fun (_,a) (_,b) -> 
	    let c = manhattan rider.tile a in 
	    let d = manhattan rider.tile b in 
	    compare c d) field_range) in
	  
	  (if (List.length field_min) > 0
	   then
	      let field_star = 
		(a_star rider.tile 
		   (let (_,z)=(List.hd field_min) in z) 
		   tail_data manhattan) in
	      (moveTo rider rider.tile (if List.length field_star > 0 then 
		  (List.nth field_star 1) else rider.tile))
	   else rider.orientation)
	else rider.orientation
      (*item*)
      )
    in

    (*check the positions around, decide to get item and react*)
    List.iter 
      (fun x -> 
	let new_orientation = check_around x (check_diamond x) in 
	let _ = send_action (ChangeOrientation(x.id,new_orientation)) in
	() ) rider_list; 
  done

let () = start_bot bot


      
  


    
    
