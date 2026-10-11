(** Vertical bars with a label under each one.

    The tallest possible bar fills the region above the labels. Bars grow in
    steps of an eighth of a cell, and the value of a bar is written inside it
    when the bar is tall enough and the number fits. Bars that do not fit in
    the region are left out. *)

(** One bar of the chart. *)
type bar =
  { label : string (** Text under the bar; cut if wider than the bar. *)
  ; value : int (** How tall the bar is, compared to the maximum. *)
  }

type t

(** Builds a bar chart.

    @param block Frame drawn around the chart; none by default.
    @param style Style of the whole region; the default style if omitted.
    @param bar_style Colors of the bars; the default style if omitted.
    @param bar_width Cells a bar is wide; three by default.
    @param bar_gap Cells between two bars; one by default.
    @param maximum The value that gives a full-height bar; the largest value
      of the chart by default. Larger values are cut to the full height.
    @param bars The bars, from left to right.
    @return The new chart.
    @raise Invalid_argument if a value or the maximum is negative, if the bar
      width is below one or if the gap is negative. *)
val make
  :  ?block:Block.t
  -> ?style:Style.t
  -> ?bar_style:Style.t
  -> ?bar_width:int
  -> ?bar_gap:int
  -> ?maximum:int
  -> bar list
  -> t

(** Turns a bar chart into a widget that draws it.

    @param chart The chart to draw.
    @return A widget that draws the chart in the region it receives. *)
val widget : t -> Widget.t
