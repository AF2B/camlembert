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

(** Asks the terminal to report the mouse: button presses and releases, the
    wheel, and moves while a button is held down. *)
val enable_mouse : string

(** Stops the terminal from reporting the mouse. *)
val disable_mouse : string

(** Asks the terminal to mark pasted text, so it arrives as a single paste
    instead of as key presses. *)
val enable_bracketed_paste : string

(** Goes back to receiving pasted text as key presses. *)
val disable_bracketed_paste : string

(** Blanks the screen and moves the cursor to the top-left corner. *)
val clear_screen : string

(** Goes back to the default colors and text effects. *)
val reset_style : string

(** Blanks the screen from a cell to the end, reading row by row, and leaves
    the cursor on that cell.

    @param column Column of the first cell to blank, counted from zero.
    @param row Row of the first cell to blank, counted from zero.
    @return The sequence that does it. *)
val clear_from : column:int -> row:int -> string

(** Scrolls the screen up by printing line feeds on its last row, which makes
    the terminal keep the rows that leave through the top in its history.

    @param rows How many rows to scroll; nothing is sent for zero or less.
    @param last_row Last row of the screen, counted from zero.
    @return The sequence that does it. *)
val scroll_up : rows:int -> last_row:int -> string

(** Asks the terminal to report where its cursor is. The answer arrives as
    input, and {!find_cursor_report} reads it. *)
val request_cursor_position : string

(** Finds the answer to {!request_cursor_position} in bytes read from the
    terminal, which may hold other things before and after it.

    @param input The bytes read from the terminal.
    @return The position of the cursor, or [None] when there is no complete
      report in the input. *)
val find_cursor_report : string -> Camlembert.Position.t option

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
