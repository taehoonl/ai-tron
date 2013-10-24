open Definitions
open Constants
open Util

type cell = Tail of color | Item of item | Rider of int
type game_state =
  {
    mutable game_data : game_data;
    mutable board : cell list array;
    mutable tails : tile list;
  }

let tile_to_cell (col,row) : int = 
  row*cNUM_COLUMNS + col

let cell_to_tile ind : int*int =
  (ind mod cNUM_COLUMNS, ind / cNUM_COLUMNS)

let state_create col row : game_state =
  (* init game data with (empty rider list & init item list) *)
  let initial_game_data =
    let items = [(Shield, cNUM_INITIAL_TEAM_SHIELD);
		   (Invincibility,cNUM_INITIAL_TEAM_INVINCIBILITY)] in
    let teamRed = ([],items) in
    let teamBlue = ([], items) in
    let fieldItem = [] in
    (teamRed, teamBlue, fieldItem)
  in
  { 
    game_data = initial_game_data;  
    board = Array.make (row*col) [];
		tails = [];
  }

let updateCell g tl cell: unit =
  let old = g.board.(tile_to_cell tl) in
  g.board.(tile_to_cell tl) <- cell::old
	
let checkCellEmpty g tl: bool =
  g.board.(tile_to_cell tl) = []

let deleteCell g tl cell_type: unit =
  let delete_helper x =
    match (x,cell_type) with
    | (Item t1, Item t2) -> if t1 = t2 then false else true
    | (Rider id1, Rider id2) -> if id1 = id2 then false else true
    | (Tail color1, Tail color2) -> if color1 = color2 then false else true
    | _ -> true
  in
  let old_cell = g.board.(tile_to_cell tl) in
  let new_cell = List.filter delete_helper old_cell in
  g.board.(tile_to_cell tl) <- new_cell
	
let addItem g itm: unit = 
  let (t1,t2,f_item) = g.game_data in
  let rec pos () = 
    let tl = ((get_random_num (cNUM_COLUMNS-1)), (get_random_num (cNUM_ROWS-1))) in
    if checkCellEmpty g tl 
    then (updateCell g tl (Item itm); g.game_data <- (t1,t2,((itm,tl)::f_item)))
    else pos ()  
  in
  pos ()

let deleteItem g tl: unit = 
  let (t1,t2,f_item) = g.game_data in
  deleteCell g tl (Item(Shield));
  deleteCell g tl (Item(Invincibility));
  let new_f_item = List.filter (fun (a,b) -> not (b = tl)) f_item in
  g.game_data <- (t1,t2,new_f_item)

let addRider g rider =
  let color = if (rider.id mod 2 = 0) then Red else Blue in
  let ((r_team,r_item), (b_team,b_item), field_item) = g.game_data in
     (* check if the rider id already exists*)
  let exist =
    if color = Red then List.exists (fun x -> x.id = rider.id) r_team
    else List.exists (fun x -> x.id = rider.id) b_team 
  in
  (if not exist then 
      ((* add to the board *)
	updateCell g rider.tile (Rider(rider.id));
	let new_game_data = 
          (* add to team data *)
	  if color = Red
	  then ((rider::r_team,r_item), (b_team,b_item), field_item)
	  else ((r_team,r_item), (rider::b_team,b_item), field_item) 
	in
	g.game_data <- new_game_data)
   else ())

let deleteRider g id =
  let ((r_team,r_item), (b_team,b_item), f) = g.game_data in
  let color = if id mod 2 = 0 then Red else Blue in
  let team = if color = Red then r_team else b_team in
  let exist = List.exists (fun r -> r.id = id) team in
  if exist then
    (let rider = List.find (fun r -> r.id = id) team in
    (* unset Board cell to Empty*)
    deleteCell g rider.tile (Rider(id));
    (* Delete from the game data *)
    let new_game_data = 
      (((List.filter (fun r -> not (r.id = id)) r_team),r_item), 
       ((List.filter (fun r -> not (r.id = id)) b_team),b_item), f)
    in
    g.game_data <- new_game_data)
  else ()

let getGameData g : game_data = 
  g.game_data

let getFieldItems g : field_items_data =
  let (_,_,f_items) = g.game_data in 
  f_items

let getTeamData g c : team_data =
  let (red_team, blue_team, _) = g.game_data in
  if c = Red then red_team else blue_team
	
let getBoard g :cell list array = g.board

let updateTeamData g c new_data : unit = 
  let (r_team, b_team, f_item) = g.game_data in
  let new_game_data = 
    if c = Red then (new_data, b_team, f_item) else (r_team, new_data, f_item)
  in
  g.game_data <- new_game_data  

let getRiderColor g id : color option =
  let ((r_riders,_), (b_riders,_), _) = g.game_data in
  let ret = 
    if id mod 2 = 0 
    then List.exists (fun x -> x.id = id) r_riders
    else List.exists (fun x -> x.id = id) b_riders
  in
  if ret then (if id mod 2 = 0 then Some Red else Some Blue)
  else None

let getTeamItem g c item : int =
  let ((_,ri), (_,bi), f_item) = g.game_data in
  let (_,n) = List.find (fun (i,n) -> i = item) (if c = Red then ri else bi) in
  n

let checkRiderItem g id item: bool = 
  let modi = if item = Shield then Shielded else Invincible in
  let ((r_riders,_), (b_riders,_), _) = g.game_data in
  let riders = if id mod 2 = 0 then r_riders else b_riders in
  if List.exists (fun x -> x.id = id) riders 
  then (
    let rider = List.find (fun x -> x.id = id) riders in 
    List.exists (fun x -> x = modi) rider.modifiers)
  else false

let item_to_modi item : modifier =
  if item = Shield then Shielded else Invincible
  
let getIdList (g:game_state):int list =
  let ((r1,_),(r2,_),_) = getGameData g in
  let x = List.fold_left (fun acc y -> (y.id)::acc) [] r1 in
  List.fold_left (fun acc y -> (y.id)::acc) x r2
    
let getRider g id:rider =
  let ((r1,_),(r2,_),_) = getGameData g in
  try List.find (fun x-> x.id=id) (if id mod 2 = 0 then r1 else r2) with
  | _ -> failwith "getRider Not found"
    
	
let moveRiders g:unit =
  let ((r1,i1),(r2,i2),f_item) = getGameData g in
  let r1 = List.map (fun x ->	
    let ori=x.orientation and (c,r) = x.tile in
		(g.tails <- ((x.tile)::(g.tails)));
		updateCell g (c,r) (Tail Red);
    deleteCell g (c,r) (Rider x.id);
    let tl = 
  	match ori with 
  	| North -> updateCell g (c,r-1) (Rider x.id); (c,r-1)
  	| South -> updateCell g (c,r+1) (Rider x.id); (c,r+1)
  	| East ->  updateCell g (c+1,r) (Rider x.id); (c+1,r)
  	| West ->  updateCell g (c-1,r) (Rider x.id); (c-1,r) 
    in
    {id=x.id;
     orientation=ori;
     modifiers=x.modifiers;
     tile=tl;
     invincibility_timer=x.invincibility_timer}) r1
  in
  let r2 = List.map (fun x ->	
    let ori=x.orientation and (c,r) = x.tile in
		(g.tails <- ((x.tile)::(g.tails)));
    updateCell g (c,r) (Tail Blue);
    deleteCell g (c,r) (Rider x.id);
    let tl = 
      match ori with 
  	| North -> updateCell g (c,r-1) (Rider x.id); (c,r-1)
  	| South -> updateCell g (c,r+1) (Rider x.id); (c,r+1)
  	| East ->  updateCell g (c+1,r) (Rider x.id); (c+1,r)
  	| West ->  updateCell g (c-1,r) (Rider x.id); (c-1,r) 
    in
    {id=x.id;
     orientation=ori;
     modifiers=x.modifiers;
     tile=tl;
     invincibility_timer=x.invincibility_timer}) r2 in
  g.game_data <- ((r1,i1),(r2,i2),f_item)

let checkTileInbound tl : bool =
  let (c,r)=tl in
  (c<cNUM_COLUMNS && r<cNUM_ROWS && c >= 0 && r >= 0)
	
let getIdCellList g id: cell list = 
  let tl = (getRider g id).tile in
  g.board.(tile_to_cell tl)
	
let rec getCellItem lst:item option = 
  match lst with
  | [] -> None
  | (Item item)::tl -> Some item
  | _::tl -> getCellItem tl

let addTeamItem g id itm:unit = 
  let ((r1,i1),(r2,i2),f_item) = g.game_data in
  let rider = getRider g id in
  let f_item = List.filter (fun (i,tl) -> not(tl = (rider.tile) )) f_item in
  (if (id mod 2) = 0 then 
      let i1 = List.map (fun (i,n) -> if i=itm then (i,n+1) else (i,n)) i1 in
      g.game_data <- ((r1,i1),(r2,i2),f_item)
    else 
      let i2 = List.map (fun (i,n) -> if i=itm then (i,n+1) else (i,n)) i2 in
      g.game_data <- ((r1,i1),(r2,i2),f_item))


let updateInvincibility g : unit =
  let ((r1,i1),(r2,i2),f_item) = g.game_data in
  let r1 = List.map (fun x -> 
    let t = x.invincibility_timer in
    let inv = if t>0 then t-1 else t in
    let modi = if inv>0 then x.modifiers else List.filter 
	(fun a -> not(a=Invincible)) x.modifiers in
    {id=x.id;
     orientation=x.orientation;
     modifiers=modi;
     tile=x.tile;
     invincibility_timer=inv}) r1 
  in
  let r2 = List.map (fun x -> 
    let t = x.invincibility_timer in
    let inv = if t>0 then t-1 else t in
    let modi = if inv>0 then x.modifiers else List.filter 
	(fun a -> not(a=Invincible)) x.modifiers in
    {id=x.id;
     orientation=x.orientation;
     modifiers=modi;
     tile=x.tile;
     invincibility_timer=inv}) r2 
  in
  g.game_data <- ((r1,i1),(r2,i2),f_item)
	
let partition g lst = 
  let rec helper l acc =
    let (inv,sh,none,tail,item) = acc in
    match l with
    | [] -> acc
    | (Tail c)::tl -> helper tl (inv,sh, none,((Tail c)::tail),item)
    | (Rider id)::tl -> 
      if (checkRiderItem g id Invincibility) then 
	helper tl (((Rider id)::inv),sh,none,tail,item)
      else if (checkRiderItem g id Shield) then  
	helper tl (inv,((Rider id)::sh),none,tail,item)
      else helper tl (inv,sh,((Rider id)::none),tail,item)
    | (Item i)::tl-> helper tl (inv,sh,none,tail,((Item i)::item))
  in
  helper lst ([],[],[],[],[])
	
let riderToInt r = match r with
  | Rider x -> x
  | _ ->failwith"rier to int fail"

let itemToItem it = match it with
  | Item x -> x
  | _ -> failwith" itemToItem fail"

let isItem x = match x with
  | Item x -> true
  | _ -> false

let moveTile tl ori:tile =
  let (c,r) = tl in
  match ori with
  | North -> (c,r-1)
  | South -> (c,r+1)
  | East -> (c+1,r)
  | West -> (c-1,r)

let getRiderIndex g id:int =
  (tile_to_cell ((getRider g id).tile))
	
let removeModifier g id m : unit =
  let ((r1,i1),(r2,i2),f_item) = g.game_data in
  let team = if (id mod 2 = 0) then r1 else r2 in
  let new_t = List.map (fun x -> 
    if x.id = id
    then (let new_mod = List.filter (fun y -> not(y = m)) x.modifiers in
	  {id = x.id; 
	   orientation = x.orientation; 
	   modifiers = new_mod; 
	   tile = x.tile;
	   invincibility_timer = x.invincibility_timer;})
    else x ) team in
  let new_data = 
    if id mod 2 = 0 then 
      ((new_t,i1),(r2,i2),f_item) 
    else ((r1,i1),(new_t,i2),f_item) in
  g.game_data <- new_data

let addModifier g id m : unit =
  let ((r1,i1),(r2,i2),f_item) = g.game_data in
  let team = if (id mod 2 = 0) then r1 else r2 in
  let new_t = List.map (fun x -> 
    if x.id = id || List.mem m x.modifiers
    then ({id = x.id; 
	   orientation = x.orientation; 
	   modifiers = m::x.modifiers; 
	   tile = x.tile;
	   invincibility_timer = (if m = Invincible then 
	       cINVINCIBILITY_TIME else x.invincibility_timer);})
    else x ) team in
  let new_data = 
    if id mod 2 = 0 then 
      ((new_t,i1),(r2,i2),f_item) 
    else ((r1,i1),(new_t,i2),f_item) in
  g.game_data <- new_data
	
let deleteTail g ind = 
  (g.tails <- List.filter (fun x -> not((cell_to_tile ind)=x)) g.tails);
  g.board.(ind) <- (List.filter (fun x ->
    (not(Tail Red=x) && not(Tail Blue=x))) g.board.(ind))
	
let getTails g = g.tails
