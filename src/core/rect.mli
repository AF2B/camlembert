(** A rectangular region of the terminal, measured in character cells.

    The origin is the top-left corner of the terminal. Columns grow to the
    right and rows grow downwards. *)

type t =
  { column : int (** Leftmost column of the rectangle. *)
  ; row : int (** Topmost row of the rectangle. *)
  ; width : int (** Number of columns the rectangle spans. *)
  ; height : int (** Number of rows the rectangle spans. *)
  }

(** Builds a rectangle.

    @param column Leftmost column of the rectangle.
    @param row Topmost row of the rectangle.
    @param width Number of columns the rectangle spans.
    @param height Number of rows the rectangle spans.
    @return The new rectangle.
    @raise Invalid_argument if any parameter is negative. *)
val make : column:int -> row:int -> width:int -> height:int -> t

(** Number of cells inside a rectangle.

    @param rect The rectangle to measure.
    @return [width * height]. *)
val area : t -> int

(** Column just after the last column of a rectangle.

    @param rect The rectangle to measure.
    @return [column + width], which lies outside the rectangle. *)
val right : t -> int

(** Row just after the last row of a rectangle.

    @param rect The rectangle to measure.
    @return [row + height], which lies outside the rectangle. *)
val bottom : t -> int

(** Tells whether a rectangle contains no cell.

    @param rect The rectangle to test.
    @return [true] when the width or the height is zero. *)
val is_empty : t -> bool

(** Tells whether a position lies inside a rectangle.

    @param rect The rectangle to look into.
    @param column Column of the position.
    @param row Row of the position.
    @return [true] when the position is inside [rect]. *)
val contains : t -> column:int -> row:int -> bool

(** Overlapping region of two rectangles.

    @param first One of the rectangles.
    @param second The other rectangle.
    @return The shared region, empty when the rectangles do not overlap. *)
val intersection : t -> t -> t

(** Removes a margin from each side of a rectangle.

    @param rect The rectangle to shrink.
    @param top Rows removed from the top.
    @param bottom Rows removed from the bottom.
    @param left Columns removed from the left.
    @param right Columns removed from the right.
    @return The inner rectangle, empty when the margins exceed the size.
    @raise Invalid_argument if any margin is negative. *)
val shrink : t -> top:int -> bottom:int -> left:int -> right:int -> t

(** Visits every position of a rectangle, row by row.

    @param rect The rectangle to walk through.
    @param visit Called once for each position. *)
val iter_positions : t -> (column:int -> row:int -> unit) -> unit

(** Structural equality.

    @param first One rectangle.
    @param second The other rectangle.
    @return [true] when all four fields match. *)
val equal : t -> t -> bool

(** Pretty-printer, handy for test failures.

    @param formatter Where to print.
    @param rect The rectangle to print. *)
val pp : Format.formatter -> t -> unit
