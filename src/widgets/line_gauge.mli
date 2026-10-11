(** A progress bar drawn as a thin line, with a label before it.

    The label comes first, then a space, and the line takes the rest of the
    first row of the region: a thick part for what is done, followed by a thin
    part for what remains, so the bar can be read even without colors. The
    thick part grows one cell at a time. The other rows of the region are left
    as they are. *)

type t

(** Builds a line gauge.

    @param block Frame drawn around the gauge; none by default.
    @param style Style of the whole region; the default style if omitted.
    @param filled_style Style of the thick part of the line; the default style
      if omitted.
    @param unfilled_style Style of the thin part of the line; the default
      style if omitted.
    @param label Text shown before the line; the percentage by default.
    @param label_style Style layered over the label; none by default.
    @param ratio How much of the line is filled, from zero to one.
    @return The new gauge.
    @raise Invalid_argument if [ratio] is not between zero and one. *)
val make
  :  ?block:Block.t
  -> ?style:Style.t
  -> ?filled_style:Style.t
  -> ?unfilled_style:Style.t
  -> ?label:Line.t
  -> ?label_style:Style.t
  -> ratio:float
  -> unit
  -> t

(** Turns a line gauge into a widget that draws it.

    @param gauge The gauge to draw.
    @return A widget that draws the gauge in the region it receives. *)
val widget : t -> Widget.t
