(** A fake terminal that keeps its screen in memory.

    It lets widgets and whole frames be tested without a real terminal. *)

type t

(** Creates a fake terminal with a blank screen.

    @param width Number of columns of the screen.
    @param height Number of rows of the screen.
    @return The new fake terminal. *)
val create : width:int -> height:int -> t

(** The backend to give to {!Terminal.create}.

    @param test_backend The fake terminal to expose.
    @return A backend that draws on the fake screen. *)
val backend : t -> Backend.t

(** The cells currently shown on the fake screen.

    @param test_backend The fake terminal to inspect.
    @return Its screen. *)
val screen : t -> Buffer.t

(** The fake screen as plain text, ignoring styles.

    @param test_backend The fake terminal to inspect.
    @return One string per row. *)
val to_lines : t -> string list

(** How many cells were sent to the fake terminal since it was created.

    @param test_backend The fake terminal to inspect.
    @return The total count of drawn cells. *)
val drawn_cells : t -> int

(** Where the fake terminal shows its text cursor.

    @param test_backend The fake terminal to inspect.
    @return The position of the cursor, or [None] when it is hidden. *)
val cursor : t -> Position.t option

(** Simulates the user resizing the terminal window; the screen goes blank.

    @param test_backend The fake terminal to resize.
    @param width New number of columns.
    @param height New number of rows. *)
val resize : t -> width:int -> height:int -> unit
