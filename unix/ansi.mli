(** ANSI escape sequences, the language terminals understand.

    Every value here is a plain string to write to the terminal, so none of it
    touches the outside world. *)

(** Switches to the alternate screen, leaving the shell output untouched. *)
val enter_alternate_screen : string

(** Switches back from the alternate screen. *)
val leave_alternate_screen : string

(** Hides the text cursor. *)
val hide_cursor : string

(** Shows the text cursor. *)
val show_cursor : string

(** Blanks the screen and moves the cursor to the top-left corner. *)
val clear_screen : string

(** Goes back to the default colors and text effects. *)
val reset_style : string

(** Moves the cursor.

    @param column Column to move to, counted from zero.
    @param row Row to move to, counted from zero.
    @return The sequence that moves the cursor. *)
val move_cursor : column:int -> row:int -> string

(** Shows the text cursor at a position, or hides it.

    @param position The cell the cursor goes to, or [None] to hide the cursor.
    @return The sequence that does it. *)
val encode_cursor : Camlembert.Position.t option -> string

(** Selects colors and text effects for the text that follows.

    @param style The style to select.
    @return The sequence that selects it, or an empty string for a style that
      changes nothing. *)
val select_style : Camlembert.Style.t -> string

(** Turns changed cells into the bytes that show them.

    The cursor is moved only where the next cell is not already under it, the
    style is selected only when it changes, and the style is reset at the end.

    @param changes The cells to show, in the order to write them.
    @return The text to send to the terminal, empty when nothing changed. *)
val encode_changes : Camlembert.Buffer.change list -> string
