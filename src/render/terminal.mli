(** Draws frames on a backend, sending only what changed since the last one.

    A terminal keeps two buffers: the frame being composed and the frame
    currently on the screen. After every draw it sends the difference between
    them to the backend.

    The frames can fill the whole screen, or only some rows of it. An inline
    viewport leaves the lines already on the screen where they are and takes
    its rows below the cursor, so the program can show a live area, such as a
    progress bar, in the middle of a shell session. *)

(** Which part of the screen the frames are drawn in. *)
type viewport =
  | Fullscreen (** The whole screen. *)
  | Inline of int
  (** This many rows, starting at the line of the cursor, with everything
      above them left alone. The screen scrolls when there is not enough room
      below the cursor. *)

type t

(** Creates a terminal on a backend, assuming the screen is blank.

    An inline viewport starts on the line of the cursor, or on the next one
    when the cursor is in the middle of a line, so the text before it is
    kept. It is never taller than the screen. The rows below the start are
    blanked.

    @param viewport Which part of the screen to draw in; [Fullscreen] by
      default.
    @param backend Where the frames are shown.
    @return The new terminal.
    @raise Invalid_argument if an inline viewport has less than one row. *)
val create : ?viewport:viewport -> Backend.t -> t

(** The region of the screen the terminal draws in.

    @param terminal The terminal to inspect.
    @return The area frames are drawn in: the whole screen, or the rows of an
      inline viewport. It is as of creation or of the last draw. *)
val area : t -> Rect.t

(** Composes one frame and shows it.

    When the backend was resized since the last draw, the viewport is cleared
    and the whole frame is sent again; an inline viewport is also placed again
    from the position of the cursor. The text cursor is shown only if the frame
    asks for it with {!Frame.set_cursor}. When it is hidden, an inline viewport
    keeps it on its first row.

    @param terminal The terminal to draw on.
    @param render_frame Draws the frame, usually by rendering widgets. *)
val draw : t -> (Frame.t -> unit) -> unit

(** Forgets what is on the screen, so the next draw sends the whole frame.

    Only the viewport is blanked, so the lines above an inline viewport stay.

    @param terminal The terminal to clear. *)
val clear : t -> unit

(** Leaves the terminal ready for whatever is printed next.

    For an inline viewport the last frame stays on the screen and the cursor
    goes to the start of the line below it, scrolling the screen if the frame
    is at the bottom. Nothing happens for a full screen. Call it once, when the
    program is done drawing.

    @param terminal The terminal to leave. *)
val finish : t -> unit
