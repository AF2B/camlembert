(** A widget that shows text, optionally framed by a block.

    Each line of the text goes on its own row, unless wrapping is asked for.
    Rows longer than the room are cut, and rows beyond the bottom are left out.
    The rows can be scrolled to show later ones. *)

type t

(** Builds a paragraph.

    @param block Frame drawn around the text; none by default.
    @param style Style of the whole region; the default style if omitted.
    @param alignment How lines sit in the room unless they set their own;
      [Left] by default.
    @param wrap How long lines are broken into several rows; lines are not
      broken, only cut, when omitted.
    @param scroll Rows hidden above the first one shown, counted after
      wrapping; zero by default.
    @param text The lines to show.
    @return The new paragraph.
    @raise Invalid_argument if [scroll] is negative. *)
val make
  :  ?block:Block.t
  -> ?style:Style.t
  -> ?alignment:Alignment.t
  -> ?wrap:Wrap.mode
  -> ?scroll:int
  -> Text.t
  -> t

(** Counts the rows the text takes.

    Scrolling and the block are not counted. It tells how far a paragraph can
    be scrolled.

    @param paragraph The paragraph to measure.
    @param width Columns of the room the text is shown in.
    @return The number of rows, after wrapping if the paragraph wraps. *)
val line_count : t -> width:int -> int

(** Turns a paragraph into a widget that draws it.

    @param paragraph The paragraph to draw.
    @return A widget that draws the paragraph in the region it receives. *)
val widget : t -> Widget.t
