(** A grid of cells that widgets draw into.

    A buffer is the screen being composed: a whole frame is drawn into it and
    then compared with the previous frame, so only the cells that changed are
    sent to the terminal. It is mutable on purpose, because the grid is
    rewritten on every frame. It mirrors Ratatui's [Buffer], so inside this
    library it shadows the standard [Buffer] module. *)

type t

(** A cell that differs from the previous frame. *)
type change =
  { column : int (** Column of the cell on the screen. *)
  ; row : int (** Row of the cell on the screen. *)
  ; cell : Cell.t (** New content of the cell. *)
  }

(** Creates a buffer filled with blank cells.

    @param area The region of the screen the buffer covers.
    @return The new buffer. *)
val create : Rect.t -> t

(** The region of the screen a buffer covers.

    @param buffer The buffer to inspect.
    @return Its area. *)
val area : t -> Rect.t

(** Reads one cell.

    @param buffer The buffer to read from.
    @param column Column of the cell on the screen.
    @param row Row of the cell on the screen.
    @return The cell at that position.
    @raise Invalid_argument if the position is outside of the buffer. *)
val get : t -> column:int -> row:int -> Cell.t

(** Writes text, one grapheme cluster after another.

    Nothing is written outside of the buffer. Writing stops at the first
    cluster that does not fit before [max_width], so a wide character is never
    cut in half. Control characters are skipped. The style is layered over the
    styles the cells already have.

    @param buffer The buffer to write into.
    @param column Column where the text starts.
    @param row Row where the text goes.
    @param max_width Most columns the text may take, counted from [column].
    @param style Style layered over the written cells.
    @param text The UTF-8 text to write. *)
val set_string
  :  t
  -> column:int
  -> row:int
  -> max_width:int
  -> style:Style.t
  -> string
  -> unit

(** Layers a style over every cell of a region.

    @param buffer The buffer to change.
    @param region The cells to style; the part outside the buffer is ignored.
    @param style The style layered over the existing ones. *)
val set_style : t -> Rect.t -> Style.t -> unit

(** Turns every cell back into a blank cell.

    @param buffer The buffer to clear. *)
val reset : t -> unit

(** Finds the cells that differ between two frames.

    Cells hidden under a wide character are left out, and cells touched by a
    wide character that disappeared are included, so applying the changes to
    the previous frame gives the next one.

    @param previous The frame currently on the screen.
    @param next The frame to show.
    @return The changed cells, row by row.
    @raise Invalid_argument if the buffers cover different areas. *)
val diff : previous:t -> next:t -> change list

(** Writes changed cells into a buffer, like a terminal would show them.

    @param buffer The buffer to update.
    @param changes The cells to write.
    @raise Invalid_argument if a change is outside of the buffer. *)
val apply : t -> change list -> unit

(** Renders the symbols of a buffer as plain text, ignoring styles.

    @param buffer The buffer to render.
    @return One string per row. *)
val to_lines : t -> string list
