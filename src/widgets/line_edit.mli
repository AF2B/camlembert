(** A single line of text being edited, with a cursor.

    The cursor sits between two characters, a character being what a user sees
    as one: a letter with its accents, a wide ideograph, an emoji sequence.
    Characters that take no column, such as line feeds, cannot be put in the
    line. The state also remembers how far the line is scrolled, so a line
    longer than its field keeps the cursor in view. *)

type t

(** Creates a line.

    @param text What the line holds to begin with; nothing by default. The
      cursor starts at its end.
    @return The new line. *)
val create : ?text:string -> unit -> t

(** The whole text of the line.

    @param edit The line to read.
    @return Its text, including the part scrolled out of view. *)
val text : t -> string

(** Replaces the whole text and puts the cursor at its end.

    @param edit The line to change.
    @param text The new text. *)
val set_text : t -> string -> unit

(** Empties the line.

    @param edit The line to change. *)
val clear : t -> unit

(** Where the cursor is.

    @param edit The line to inspect.
    @return How many characters come before the cursor. *)
val cursor : t -> int

(** Writes text at the cursor, which ends up after it.

    @param edit The line to change.
    @param text What to write; characters that take no column are dropped. *)
val insert : t -> string -> unit

(** Deletes the character before the cursor, if any.

    @param edit The line to change. *)
val delete_before : t -> unit

(** Deletes the character after the cursor, if any.

    @param edit The line to change. *)
val delete_after : t -> unit

(** Deletes everything before the cursor.

    @param edit The line to change. *)
val delete_to_start : t -> unit

(** Deletes everything after the cursor.

    @param edit The line to change. *)
val delete_to_end : t -> unit

(** Moves the cursor one character to the left, stopping at the start.

    @param edit The line to change. *)
val move_left : t -> unit

(** Moves the cursor one character to the right, stopping at the end.

    @param edit The line to change. *)
val move_right : t -> unit

(** Moves the cursor to the start of the line.

    @param edit The line to change. *)
val move_to_start : t -> unit

(** Moves the cursor to the end of the line.

    @param edit The line to change. *)
val move_to_end : t -> unit

(** Applies an event if it is an editing action.

    Characters and pastes are written. Left, Right, Home, End, Backspace and
    Delete work as usual, and so do the readline shortcuts Ctrl+A, Ctrl+E,
    Ctrl+B, Ctrl+F, Ctrl+H, Ctrl+D, Ctrl+K and Ctrl+U. Anything else, such as
    Enter, Tab or characters typed with Alt, is left for the application.

    @param edit The line to change.
    @param event The event to apply.
    @return [true] when the event was an editing action. *)
val handle : t -> Event.t -> bool

(** Scrolls the line, if needed, so that the cursor fits in a field.

    Field widgets call this while they draw. The line only scrolls as far as
    needed, so the text does not jump while the cursor moves inside the view.

    @param edit The line to scroll.
    @param width Columns of the field; below one counts as one. *)
val reveal : t -> width:int -> unit

(** The part of the text that is in view.

    @param edit The line to read.
    @param width Columns of the field.
    @return The text from the scrolling offset on, as much as fits. *)
val visible : t -> width:int -> string

(** The column of the cursor inside the part in view.

    @param edit The line to inspect.
    @return Columns between the left edge of the view and the cursor. *)
val cursor_column : t -> int
