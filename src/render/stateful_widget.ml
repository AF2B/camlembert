(** A widget that remembers something between frames.

    It receives the state to read and update while it draws, such as the
    selected row of a list. Like {!Widget.t}, it is a plain function. *)

type 'state t = Rect.t -> Buffer.t -> 'state -> unit
