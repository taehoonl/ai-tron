open Definitions

type cell
type game_state

(* convert from tile to 1D array index *)
val tile_to_cell: int*int -> int

(* convert from 1D array index to tile *)
val cell_to_tile: int -> int*int

(* create an empty board size of row and col and init team items *)
val state_create: int -> int -> game_state

(* add cell type to tile *)
val updateCell: game_state -> tile -> cell -> unit

(* return true if tile's cell is empty *)
val checkCellEmpty: game_state -> tile -> bool

(* delete specified cell type from tile *)
val deleteCell: game_state -> tile -> cell -> unit

(* add an item to game state, no two items in the same tile *)
val addItem: game_state -> item -> unit 

(* delete item from specified tile *)
val deleteItem: game_state -> tile -> unit

(* add a rider to game state*)
val addRider: game_state -> rider -> unit

(* given the id, delete the corresponding rider from game state *)
val deleteRider: game_state -> int -> unit

(* return game data*)
val getGameData : game_state -> game_data

(* return field item data *)
val getFieldItems : game_state -> field_items_data

(* return team data of specified color *)
val getTeamData: game_state -> color -> team_data

(* return game board *)
val getBoard: game_state -> cell list array

(* update the team data of specified color *)
val updateTeamData: game_state -> color -> team_data -> unit

(* return color option of specified id. return none if rider doesnt exist*)
val getRiderColor : game_state -> int -> color option

(* return # of item of specified team and item *)
val getTeamItem : game_state -> color -> item -> int

(* return true if the rider has the specified item in its modifiers *)
val checkRiderItem : game_state -> int -> item -> bool

(* convert item to modifier *)
val item_to_modi : item -> modifier

(* return id list of all the current riders *)
val getIdList : game_state -> int list

(* return rider of speicified id *)
val getRider : game_state -> int -> rider

(* move rider in the direction of its orientation & update the new game state *)
val moveRiders : game_state -> unit

(* true if the tile is within the game bound. false otherwise *)
val checkTileInbound : tile -> bool

(* return the cell list of specified rider(it's position) *)
val getIdCellList : game_state -> int -> cell list

(* return the item option in cell. return none if there's no cell *)
val getCellItem : cell list -> item option

(* add item to the team that rider id belongs to *)
val addTeamItem : game_state -> int -> item -> unit

(* check and update riders' invincibility modifier - remove if expired*)
val updateInvincibility : game_state -> unit

val partition :game_state->cell list->(cell list*cell list*cell list*cell list*cell list)

(* return the rider id in specified cell *)
val riderToInt : cell -> int

(* convert cell type to item *)
val itemToItem : cell -> item

(* true if cell type is an item. false otherwise *)
val isItem : cell ->bool

(* return the next tile in the orientation *)
val moveTile : tile -> orientation -> tile

(* get rider's cell index *)
val getRiderIndex : game_state -> int -> int

(* remove specified modifier from the specified rider *)
val removeModifier : game_state -> int -> modifier -> unit

(* add specified modifier to the specified rider *)
val addModifier : game_state -> int -> modifier -> unit

(* delete tail from cell index *)
val deleteTail : game_state -> int ->unit

(* return tail data *)
val getTails : game_state -> tile list
