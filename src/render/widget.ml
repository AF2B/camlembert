(** Something that can draw itself.

    A widget receives the region it may draw in and the buffer to draw into.
    Widgets are plain functions, so they compose with ordinary higher-order
    functions. *)

type t = Rect.t -> Buffer.t -> unit
