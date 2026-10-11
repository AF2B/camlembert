(** A widget that remembers something between frames.

    It receives the state to read and update while it draws, such as the
    selected row of a list. Like {!Widget.t}, it is a plain function. *)

type 'state t = Rect.t -> Buffer.t -> 'state -> unit

(** Gives a state to a stateful widget, which makes it a plain widget.

    It lets a stateful widget take part in {!Widget.split}.

    @param widget The widget that needs a state.
    @param state The state it reads and updates every time it draws.
    @return A widget that draws with that state. *)
val with_state : 'state t -> 'state -> Widget.t
