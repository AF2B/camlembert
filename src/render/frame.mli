(** The frame being composed by a draw callback. *)

type t

(** Wraps the buffer of a frame.

    The terminal calls this for you; draw callbacks receive a ready frame.

    @param buffer The buffer the frame draws into.
    @return The new frame, with the text cursor hidden. *)
val create : Buffer.t -> t

(** The whole region the frame can draw in.

    @param frame The frame to inspect.
    @return Its area. *)
val area : t -> Rect.t

(** The buffer behind the frame, for drawing without a widget.

    @param frame The frame to inspect.
    @return Its buffer. *)
val buffer : t -> Buffer.t

(** Draws a widget in a region of the frame.

    @param frame The frame to draw into.
    @param widget The widget to draw.
    @param area The region the widget may use. *)
val render_widget : t -> Widget.t -> area:Rect.t -> unit

(** Asks for the text cursor to be shown at a position once the frame is drawn.

    The cursor is hidden unless every frame asks for it.

    @param frame The frame being composed.
    @param position The cell the cursor goes to. *)
val set_cursor : t -> Position.t -> unit

(** The position the frame asked the cursor to be shown at.

    @param frame The frame to inspect.
    @return The position, or [None] when the cursor is to be hidden. *)
val cursor : t -> Position.t option
