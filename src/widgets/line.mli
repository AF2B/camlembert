(** A line of text made of spans, each with its own style. *)

type t =
  { spans : Span.t list (** The pieces of the line, from left to right. *)
  ; alignment : Alignment.t option
    (** How the line sits in its room, or [None] to follow its container. *)
  }

(** Builds a line from spans.

    @param alignment How the line sits in its room; follows the container when
      omitted.
    @param spans The pieces of the line, from left to right.
    @return The new line. *)
val make : ?alignment:Alignment.t -> Span.t list -> t

(** Builds a line with a single span.

    @param style How the text is drawn; defaults to the default style.
    @param alignment How the line sits in its room; follows the container when
      omitted.
    @param content The UTF-8 text of the line.
    @return The new line. *)
val of_string : ?style:Style.t -> ?alignment:Alignment.t -> string -> t

(** Number of terminal columns the line takes.

    @param line The line to measure.
    @return The summed width of its spans. *)
val width : t -> int

(** Draws a line into a buffer, span after span.

    @param line The line to draw.
    @param buffer The buffer to draw into.
    @param column Column where the line starts.
    @param row Row where the line goes.
    @param max_width Most columns the line may take; the rest is cut. *)
val render : t -> Buffer.t -> column:int -> row:int -> max_width:int -> unit

(** Draws a line aligned inside a room, on one row.

    The alignment of the line wins over the default one, if it has one.

    @param line The line to draw.
    @param buffer The buffer to draw into.
    @param alignment How the line sits in the room unless it sets its own.
    @param column First column of the room.
    @param row Row where the line goes.
    @param width Columns of the room; what does not fit is cut. *)
val render_aligned
  :  t
  -> Buffer.t
  -> alignment:Alignment.t
  -> column:int
  -> row:int
  -> width:int
  -> unit
