open Definitions
open Constants
open Util
open State

type game = 
  {
    mutable time : float;
    state : game_state;
    mutex : Mutex.t;
  }

let initGame () : game = 
  (*create state*)
  let new_g = state_create cNUM_COLUMNS cNUM_ROWS in
  let red_col = cNUM_COLUMNS / 10 in
  let blue_col = cNUM_COLUMNS - 1 - red_col in
  let row = cNUM_ROWS / (cNUM_INITIAL_TEAM_RIDERS + 1) in

  (* RED -> Even IDs*)
  for i = 0 to cNUM_INITIAL_TEAM_RIDERS - 1 do
    let tl = (red_col, row * (i+1)) in
    let new_rider = 
      {id = i*2; 
       orientation = East; 
       modifiers = []; 
       tile = tl; 
       invincibility_timer = 0} in
    addRider new_g new_rider
  done;

  (* BLUE -> Odd IDs *)
  for i = 0 to cNUM_INITIAL_TEAM_RIDERS - 1 do
    let tl = (blue_col, row * (i+1)) in
    let new_rider = 
      {id = i*2 + 1; 
       orientation = West; 
       modifiers = []; 
       tile = tl; 
       invincibility_timer = 0} in
    addRider new_g new_rider
  done;

  let ((r_riders,_),(b_riders,_),_) = getGameData new_g in
  List.iter 
    (fun x -> Netgraphics.add_update (PlaceRider(x.id,x.tile,Red))) r_riders;
  List.iter
    (fun x -> Netgraphics.add_update (PlaceRider(x.id,x.tile,Blue))) b_riders;
  Netgraphics.send_update InitGraphics;

  let old_time = Unix.gettimeofday() in
  {
    time = old_time;
    state = new_g;
    mutex = Mutex.create()
  }

let initFieldItems g : unit = 
  for n=1 to cNUM_INITIAL_FIELD_SHIELD do
    (addItem g.state Shield);
  done;
  
  for m=1 to cNUM_INITIAL_FIELD_INVINCIBILITY do
		(addItem g.state Invincibility);
  done;
	let ((_s,red_items),(_,blue_items),f_items) = getGameData g.state in
	(Netgraphics.add_update (UpdateInventory(Red,red_items)));
	(Netgraphics.add_update (UpdateInventory(Blue,blue_items)));
	(List.iter (fun (item,tile) -> 
	  Netgraphics.add_update (PlaceItem(item,tile))) f_items)
	
let handleAction g act c : command = 
  let m = g.mutex in
  Mutex.lock m;
  let res =
    (* will involve having to get this unit_id's team color,
     * and checking it against c. Return Failed if the two
     * colors are not equal. Else, match against all the possible actions.
     *)
    match act with
    | ChangeOrientation(id,ori) -> 
      (match getRiderColor g.state id with
       | None -> Failed
       | Some y -> 
	 (if not (y=c) then Failed 
	 else
	   (let (r,i) = getTeamData g.state c in
	   let new_rider=List.map (fun z -> if not(z.id=id) then z else
	       {id=z.id;
		orientation=ori;
		modifiers=z.modifiers;
		tile=z.tile;
		invincibility_timer=z.invincibility_timer}) r 
	   in
	   updateTeamData g.state c (new_rider,i); Success)))
	      
    | UseItem (id,item) -> 
      (match getRiderColor g.state id  with
      | None -> Failed
      | Some y -> 
	if not(y=c) then Failed 
	else
	  (let (r,i) = getTeamData g.state c in
	  if (getTeamItem g.state c item) <= 0 then Failed 
	  else
	    (let new_item = List.map (fun (itm,n) -> 
	      if itm=item then (itm,n-1) else (itm,n)) i in
	     updateTeamData g.state c (r,new_item);
	     addModifier g.state id (item_to_modi item);  
	     Success)))
  in
  Mutex.unlock m;
  Result res
    
let handleStatus g status : command = 
  let m = g.mutex in
  Mutex.lock m;

  let data =
    match status with
    | TeamItemsStatus(c) -> 
      (let (_,i) = getTeamData g.state c in TeamItemsData (i))
    | FieldItemsStatus -> FieldItemsData (getFieldItems g.state)
    | TeamStatus(c) -> TeamData (getTeamData g.state c)
    | GameStatus -> GameData (getGameData g.state)
    | TailStatus -> TailData (getTails g.state)
  in
  Mutex.unlock m;
  Data(data)


let check_for_game_over g curr_time : game_result option = 
  let ((r1,_),(r2,_),_) = getGameData g.state in
  let r1 = List.length r1 and r2 = List.length r2 in
  (Netgraphics.add_update 
     (Countdown (int_of_float(cTIME_LIMIT -. (curr_time -. g.time)))));
  if (curr_time -. g.time) > cTIME_LIMIT 
  then 
    (if r1 = r2 then Some Tie 
     else if r1 > r2 then Some (Winner Red) 
     else Some (Winner Blue))
  else 
    (if r1=0 && r2=0 then Some Tie
     else if r1>0 && r2=0 then Some (Winner Red)
     else if r1=0 && r2>0 then Some (Winner Blue)
     else None)    
      

let handleTime g new_time : game_result option = 
  let m = g.mutex in
  Mutex.lock m;
  let res = check_for_game_over g new_time in
  (match res with
  | Some c -> (Netgraphics.add_update (GameOver(c)))
  | None -> 
    ((* check out of bounds + move riders + place tails *)
      let ((red_riders,_),(blue_riders,_),_) = getGameData g.state in
      (List.iter (fun x -> 
	(Netgraphics.add_update (PlaceTail (x.id,x.tile,Red)))) red_riders);
      (List.iter (fun x -> 
	(Netgraphics.add_update (PlaceTail (x.id,x.tile,Blue)))) blue_riders);
     
     let idlist = getIdList g.state in 
     List.iter (fun i -> let x=getRider g.state i in 
			 let tl=(moveTile x.tile x.orientation) in 
			 if not(checkTileInbound tl) then 
			   ((deleteRider g.state x.id); 
			    Netgraphics.add_update (RemoveRider x.id))) idlist;
     moveRiders g.state;
     
     (*get rider tiles*)
     let idlist = getIdList g.state in
     let indlist = List.fold_left (fun a i -> 
       let ind = (getRiderIndex g.state i) in 
       if List.mem ind a then a else ind::a) [] idlist in
     
     (*check each of the rider tiles*)
     List.iter (fun ind-> 
       (* partition into different cases of collisions *)
       let (inv,sh,none,tail,item)=partition g.state (getBoard g.state).(ind) in
       (*one invincible rider*)
       if List.length inv = 1 
       then 
	 begin 
	   (List.iter 
	      (fun i -> 
		(Netgraphics.add_update (RemoveRider (riderToInt i))); 
		deleteRider g.state (riderToInt i)) (sh@none));
	   deleteTail g.state ind;
	   Netgraphics.add_update (RemoveTail (cell_to_tile ind)); 
	   (getBoard g.state).(ind) <- (inv@item) 
	 end
       else
	 (List.iter 
	    (fun i -> 
	      (Netgraphics.add_update (RemoveRider (riderToInt i))); 
	      deleteRider g.state (riderToInt i)) inv;
	  if List.length sh = 1  (* one shielded rider *)
	  then 
	    (List.iter (fun i -> 
	      (Netgraphics.add_update (RemoveRider (riderToInt i))); 
	      deleteRider g.state (riderToInt i)) none;
	     (if ((List.length none) > 0) || ((List.length tail) > 0) then 
		 (removeModifier g.state (riderToInt (List.hd sh)) Shielded));
	     (deleteTail g.state ind);
	     (Netgraphics.add_update (RemoveTail (cell_to_tile ind))); 
	     (getBoard g.state).(ind) <- (sh@item))
  	  else (*  *)
  	    (List.iter (fun i -> 
	      (Netgraphics.add_update (RemoveRider (riderToInt i))); 
	      deleteRider g.state (riderToInt i))  sh;
  	     if List.length tail >= 1  
	     then List.iter (fun i -> 
	       (Netgraphics.add_update (RemoveRider (riderToInt i))); 
	       deleteRider g.state (riderToInt i)) none 
  	     else 
  	       (if List.length none = 1 
  		then ()
  		else List.iter (fun i -> 
		  (Netgraphics.add_update (RemoveRider (riderToInt i))); 
		  deleteRider g.state (riderToInt i)) none )))) 
       indlist;
     
     (*item check*)
     let idlist = getIdList g.state in
     (List.iter (fun i -> 
       let (itm,rest)= 
	 List.partition (fun x -> isItem x) (getIdCellList g.state i) in
       (getBoard g.state).(tile_to_cell (getRider g.state i).tile) <- rest;
       if List.length itm>0 
       then
  	 ((Netgraphics.add_update (RemoveItem (getRider g.state i).tile));	
  	  addTeamItem g.state i) (itemToItem (List.hd itm)) 
       else ()) idlist);
     
     (*update invincibility timer*)
     updateInvincibility g.state;
     let idlist = getIdList g.state in
     let ((red_riders,red_items),(blue_riders,blue_items),_) = 
       getGameData g.state in

     (*graphics updates*)
     (Netgraphics.add_update (UpdateInventory(Red,red_items)));
     (Netgraphics.add_update (UpdateInventory(Blue,blue_items)));
     List.iter 
       (fun id -> Netgraphics.add_update 
	 (ModifyRider (id,Shielded,(checkRiderItem g.state id Shield)))) idlist;
     List.iter 
       (fun id -> Netgraphics.add_update
	 (ModifyRider 
	    (id,Invincible,(checkRiderItem g.state id Invincibility)))) idlist;
     List.iter 
       (fun x -> Netgraphics.add_update 
	 (UpdateRider (x.id,x.orientation,x.tile))) red_riders;
     (List.iter (fun x -> 
       Netgraphics.add_update 
	 (UpdateRider (x.id,x.orientation,x.tile))) blue_riders)
    )
  );
		
  Mutex.unlock m;
  res
   
