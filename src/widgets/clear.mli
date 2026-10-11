(** A widget that blanks its region.

    Draw it before a popup, so that what is underneath does not show through
    the cells the popup leaves empty. *)

(** Blanks every cell of the region it receives, text and styles included. *)
val widget : Widget.t
