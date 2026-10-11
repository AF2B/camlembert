(** A row of titles, one of them selected.

    Titles sit side by side on the first row of the region, each with a space
    on both sides, and a divider goes between two titles. What does not fit in
    the region is cut. *)

type t

(** Builds a row of tabs.

    @param block Frame drawn around the tabs; none by default.
    @param style Style of the whole region; the default style if omitted.
    @param highlight_style Style layered over the selected title; the default
      style if omitted.
    @param divider What goes between two titles; a thin vertical line by
      default.
    @param selected Position of the selected title, counted from zero; the
      first one by default. Out of range means nothing is selected.
    @param titles The titles, from left to right.
    @return The new tabs. *)
val make
  :  ?block:Block.t
  -> ?style:Style.t
  -> ?highlight_style:Style.t
  -> ?divider:Line.t
  -> ?selected:int
  -> Line.t list
  -> t

(** Turns tabs into a widget that draws them.

    @param tabs The tabs to draw.
    @return A widget that draws the tabs in the region it receives. *)
val widget : t -> Widget.t
