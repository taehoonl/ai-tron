open Definitions
open Constants
open Util

type cell = Empty | Tail of color | Item of item | Rider of color
type game_state =
  {
    mutable game_data : game_data;
    mutable board : cell array;
  }

let tile_to_cell (col,row) : int =
  if (col < cNUM_COLUMNS && row < cNUM_ROWS) && (col >= 0 && row >= 0)
  then row*cNUM_COLUMNS + col
  else failwith "wrong tile ()"

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
    board = Array.make (row*col) Empty;
  }


let updateCell g tl cell: unit =
	let board = g.board in
	board.(tile_to_cell tl) <- cell
	
let checkCellEmpty g tl: bool =
	let board = g.board in
	Array.get board (tile_to_cell tl) == Empty 
	

let addItem g itm: unit = 
	let (t1,t2,f_item) = (g.state).game_data in
	let board = (g.state).board in
	let rec pos () = 
		let tl = ((get_rand_num (cNUM_COLUMNS-1)), (get_rand_num (cNUM_ROW-1))) in
			if checkCellEmpty g tl 
			then updateCell g tl (Item itm); g.state.game_data <- (t1,t2,((itm,tl)::f_item))
			else pos ()  

let deleteItem g tl: unit = 
	let (t1,t2,f_item) = (g.state).game_data in
	let board = (g.state).board in
	if board.(tile_to_cell tl)==Empty 
	then failwith "No item to be removed from cell"
	else updateCell g tl Empty;
	(if List.exists (fun (a,b) -> b==tl) 
	then f_item = List.filter (fun (a,b) -> not (b==tl)) f_item 
	g.state.game_data <- (team1,team2,((itm,tl)::f_item))
	else failwith "No item to be removed from field list")

let addRider state rider =
  (* check if the cell is empty *)
  if (check_cell_empty state (rider.tile))
  then
    (let color = if (rider.id mod 2 = 0) then Red else Blue in
     (* add to the board *)
     updateCell state (tile_to_cell rider.tile) Rider(color);
     let (red_team, blue_team, field_item) = state.game_data in
     let new_game_data = 
       (* add to team data *)
       if color = Red
       then (rider::red_team, blue_team, field_item)
       else (red_team, rider::blue_team, field_item) 
     in
     state.game_data <- new_game_data)
  else failwith "Add Rider -> tile is not Empty"

let deleteRider state rider =
  match (state.board.(tile_to_cell rider.tile)) with
  | Rider c -> 
    ((* unset Board cell to Empty*)
      updateCell state rider.tile Empty;
    (* Delete from the game data *)
      let (red_team, blue_team, f) = state.game_data in
      let filter_fun r = if r.id = rider.id then false else true in
      let new_game_data = 
	(List.filter filter_fun red_team, List.filter filter_fun blue_team, f)
      in
      state.game_data <- new_game_data)
  | _ -> failwith "Delete Rider -> tile is not a Rider"

