(** A bar that shows how far something has gone, with a label over it.

    The bar is as tall as its region and fills it from the left in steps of an
    eighth of a cell. The filled part is drawn with the colors of the gauge
    style swapped, so a label stays readable on top of it. *)

type t

(** Builds a gauge.

    @param block Frame drawn around the gauge; none by default.
    @param style Style of the whole region; the default style if omitted.
    @param gauge_style Colors of the bar; the default style if omitted.
    @param label Text shown in the middle of the bar; the percentage by
      default.
    @param label_style Style layered over the label; none by default.
    @param ratio How much of the bar is filled, from zero to one.
    @return The new gauge.
    @raise Invalid_argument if [ratio] is not between zero and one. *)
val make
  :  ?block:Block.t
  -> ?style:Style.t
  -> ?gauge_style:Style.t
  -> ?label:Line.t
  -> ?label_style:Style.t
  -> ratio:float
  -> unit
  -> t

(** Turns a gauge into a widget that draws it.

    @param gauge The gauge to draw.
    @return A widget that draws the gauge in the region it receives. *)
val widget : t -> Widget.t
