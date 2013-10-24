open Definitions

type cell
type game_state

(* create an empty board size of row and col and init team items *)
val state_create: int -> int -> game_state

val updateCell: game_state -> tile -> cell -> unit

val checkCellEmpty: game_state -> tile -> bool

val addItem: game_state -> item -> unit 

val deleteItem: game_state -> tile -> unit

val addRider: game_state -> tile -> rider -> unit

val deleteRider: game_state -> rider -> unit
