(** Which row of a scrolling list is selected, and which rows are in view.

    A list widget keeps one of these between frames. The application moves the
    selection, and the widget scrolls so that the selected row stays visible. *)

type t

(** Creates a state with nothing selected and no scrolling.

    @return The new state. *)
val create : unit -> t

(** The selected row.

    @param selection The state to inspect.
    @return The position of the selected row, counted from zero, or [None]. *)
val selected : t -> int option

(** Selects a row, or nothing.

    A position outside of the list is brought back into it when the list is
    drawn.

    @param selection The state to change.
    @param position The row to select, counted from zero, or [None]. *)
val select : t -> int option -> unit

(** The number of rows scrolled out of view above the first one shown.

    @param selection The state to inspect.
    @return The scrolling offset. *)
val offset : t -> int

(** Moves the selection one row down, stopping at the last row.

    Nothing selected yet means the first row gets selected.

    @param selection The state to change.
    @param item_count How many rows the list has. *)
val select_next : t -> item_count:int -> unit

(** Moves the selection one row up, stopping at the first row.

    Nothing selected yet means the last row gets selected.

    @param selection The state to change.
    @param item_count How many rows the list has. *)
val select_previous : t -> item_count:int -> unit

(** Brings the selection into the list and scrolls to keep it in view.

    List widgets call this while they draw. The selection becomes [None] for
    an empty list. The scrolling never leaves blank rows below the last row
    when the list is longer than the room.

    @param selection The state to update.
    @param item_count How many rows the list has.
    @param visible_rows How many rows fit in the room; below one counts as
      one. *)
val reveal : t -> item_count:int -> visible_rows:int -> unit
