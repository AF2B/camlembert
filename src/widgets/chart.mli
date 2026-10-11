(** A plot of datasets between two axes.

    The datasets are drawn with a {!Canvas.t} inside the part of the region
    that the axes leave free. The y axis stands on the left, with its labels
    on its left, and the x axis stands at the bottom, with its labels under it.
    The labels are given by the application and spread evenly along each axis,
    the first one at the start, the last one at the end. They are neither cut
    nor left out to keep them apart, so a narrow chart needs few and short
    labels. Whatever lies outside of the bounds of the axes is left out. *)

(** How a dataset is drawn. *)
type graph_type =
  | Scatter (** Each point on its own. *)
  | Connected (** Each point joined to the next one by a line. *)

(** Where the names of the datasets are listed, over the datasets. The list is
    left out when it would take more than a third of the width or of the height
    of the region where the datasets are drawn. *)
type legend_position =
  | Top_left
  | Top_right
  | Bottom_left
  | Bottom_right
  | Hidden

(** One of the two axes. *)
type axis

(** One series of points. *)
type dataset

type t

(** Describes an axis.

    @param title Text written at the end of the axis; none by default.
    @param labels Texts spread evenly along the axis, from the lowest value to
      the highest one; none by default.
    @param style Style of the axis line, its labels and its title; the default
      style if omitted.
    @param bounds The lowest and the highest value the axis shows.
    @return The new axis.
    @raise Invalid_argument if a bound is not a finite number or if the lowest
      one is not below the highest one. *)
val axis
  :  ?title:string
  -> ?labels:string list
  -> ?style:Style.t
  -> bounds:float * float
  -> unit
  -> axis

(** Describes a dataset.

    @param marker How finely the dataset is drawn; Braille by default.
    @param graph_type Whether points stand alone or are joined; [Scatter] by
      default.
    @param style Style of the points and of the name in the legend; the
      default style if omitted.
    @param name Name shown in the legend; an empty name keeps the dataset out
      of the legend.
    @param data The points, each one an x and a y.
    @return The new dataset. *)
val dataset
  :  ?marker:Marker.t
  -> ?graph_type:graph_type
  -> ?style:Style.t
  -> name:string
  -> (float * float) list
  -> dataset

(** Builds a chart.

    @param block Frame drawn around the chart; none by default.
    @param style Style of the whole region; the default style if omitted.
    @param legend Where the names of the datasets are listed; [Top_right] by
      default.
    @param x_axis The horizontal axis.
    @param y_axis The vertical axis.
    @param datasets The series to draw, in order; a later one is drawn over an
      earlier one.
    @return The new chart. *)
val make
  :  ?block:Block.t
  -> ?style:Style.t
  -> ?legend:legend_position
  -> x_axis:axis
  -> y_axis:axis
  -> dataset list
  -> t

(** Turns a chart into a widget that draws it.

    @param chart The chart to draw.
    @return A widget that draws the chart in the region it receives. *)
val widget : t -> Widget.t
