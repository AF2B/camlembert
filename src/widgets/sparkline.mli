(** A small chart of bars that shows how a series of numbers evolves.

    Each value is a bar one cell wide, as tall as the value is large compared
    to the maximum, in steps of an eighth of a cell. The series starts at the
    left. When there are more values than columns, the last ones are shown. *)

type t

(** Builds a sparkline.

    @param block Frame drawn around the chart; none by default.
    @param style Style of the bars and of the region; the default style if
      omitted.
    @param maximum The value that fills the whole height; the largest value
      of the series by default. Larger values are cut to the full height.
    @param values The series, oldest first.
    @return The new sparkline.
    @raise Invalid_argument if a value or the maximum is negative. *)
val make : ?block:Block.t -> ?style:Style.t -> ?maximum:int -> int list -> t

(** Turns a sparkline into a widget that draws it.

    @param sparkline The sparkline to draw.
    @return A widget that draws the sparkline in the region it receives. *)
val widget : t -> Widget.t
