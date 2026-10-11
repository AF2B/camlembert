(** A drawing surface with its own coordinates.

    Shapes are placed with ordinary numbers, the way they are in a plot: the
    lower left corner of the canvas is the point made of the lowest x and the
    lowest y, and y grows upwards. A {!Marker.t} decides how finely they are
    drawn. Whatever lies outside of the bounds is left out. Shapes are painted
    in order, so a later shape wins the colors of the cells it shares with an
    earlier one. *)

type shape =
  | Points of
      { coordinates : (float * float) list
      ; style : Style.t
      } (** Isolated points, given as an x and a y. *)
  | Segment of
      { x1 : float
      ; y1 : float
      ; x2 : float
      ; y2 : float
      ; style : Style.t
      } (** A straight line between two points. *)
  | Rectangle of
      { x : float
      ; y : float
      ; width : float
      ; height : float
      ; style : Style.t
      } (** The outline of a rectangle, from its lower left corner. *)
  | Circle of
      { x : float
      ; y : float
      ; radius : float
      ; style : Style.t
      } (** The outline of a circle, from its center. *)

(** Text written over the shapes, at a point of the canvas. *)
type label =
  { x : float (** Where the text starts, along the x axis. *)
  ; y : float (** Where the text goes, along the y axis. *)
  ; text : Line.t (** What is written. *)
  }

type t

(** Builds a canvas.

    @param block Frame drawn around the canvas; none by default.
    @param style Style of the whole region; the default style if omitted.
    @param marker How finely shapes are drawn; Braille by default.
    @param labels Text written over the shapes, after all of them; none by
      default.
    @param x_bounds The lowest and the highest x of the canvas.
    @param y_bounds The lowest and the highest y of the canvas.
    @param shapes What to draw, in order.
    @return The new canvas.
    @raise Invalid_argument if a bound is not a finite number or if the lowest
      one is not below the highest one. *)
val make
  :  ?block:Block.t
  -> ?style:Style.t
  -> ?marker:Marker.t
  -> ?labels:label list
  -> x_bounds:float * float
  -> y_bounds:float * float
  -> shape list
  -> t

(** Turns a canvas into a widget that draws it.

    @param canvas The canvas to draw.
    @return A widget that draws the canvas in the region it receives. *)
val widget : t -> Widget.t
