(** Draws frames on a backend, sending only what changed since the last one.

    A terminal keeps two buffers: the frame being composed and the frame
    currently on the screen. After every draw it sends the difference between
    them to the backend. *)

type t

(** Creates a terminal on a backend, assuming the screen is blank.

    @param backend Where the frames are shown.
    @return The new terminal. *)
val create : Backend.t -> t

(** The region of the screen the terminal draws in.

    @param terminal The terminal to inspect.
    @return The area, as of creation or of the last draw. *)
val area : t -> Rect.t

(** Composes one frame and shows it.

    When the backend was resized since the last draw, the screen is cleared and
    the whole frame is sent again.

    @param terminal The terminal to draw on.
    @param render_frame Draws the frame, usually by rendering widgets. *)
val draw : t -> (Frame.t -> unit) -> unit

(** Forgets what is on the screen, so the next draw sends the whole frame.

    @param terminal The terminal to clear. *)
val clear : t -> unit
