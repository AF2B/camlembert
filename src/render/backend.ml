(** The bridge between the library and a real, or fake, terminal.

    The library works out which cells changed; a backend is the only part that
    turns them into output. Any record of these functions is a backend. *)

type t =
  { size : unit -> Rect.t
    (** Region currently available on the screen, starting at the origin. *)
  ; cursor_position : unit -> Position.t
    (** Where the cursor stands now, whether it is shown or hidden. *)
  ; draw : Buffer.change list -> unit
    (** Shows the changed cells. The cursor is left after the last one. *)
  ; clear_from : Position.t -> unit
    (** Blanks every cell from a position to the end of the screen, reading
        row by row. The cursor goes to that position. *)
  ; scroll_up : int -> unit
    (** Moves the whole screen up by some rows. The rows that leave through the
        top stay in the history of the terminal and blank rows appear at the
        bottom. Where the cursor stands afterwards is not specified. *)
  ; move_cursor : Position.t -> unit
    (** Moves the cursor, without showing or hiding it. *)
  ; set_cursor : Position.t option -> unit
    (** Shows the text cursor at a position, or hides it. *)
  ; flush : unit -> unit (** Makes everything drawn so far visible. *)
  }
