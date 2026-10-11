(** Something that can draw itself.

    A widget receives the region it may draw in and the buffer to draw into.
    Widgets are plain functions, so they compose with ordinary higher-order
    functions. *)

type t = Rect.t -> Buffer.t -> unit

(** Makes one widget out of several, each drawn in its share of the region.

    The region is split with {!Layout.split}, so every part gets the room its
    constraint asks for.

    @param direction Whether the parts sit side by side or stacked.
    @param parts The room each part asks for, with the widget that fills it,
      in order.
    @return A widget that draws all the parts. *)
val split : direction:Layout.direction -> (Constraint.t * t) list -> t
