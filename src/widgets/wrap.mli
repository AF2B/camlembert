(** Breaking long lines of text into rows that fit a width.

    Styles are kept: a row is made of spans like the line it comes from, and it
    keeps the alignment of that line. Every line gives at least one row, even an
    empty one. Widths are counted in terminal columns, so wide characters are
    never cut in half. A character wider than the width gets a row of its own. *)

type mode =
  | Word
  (** Break between words. The spaces at a break are dropped, and a word
          longer than a row is cut. *)
  | Character (** Break wherever the row is full, spaces included. *)

(** Breaks a line into rows.

    @param mode Where the line may be broken.
    @param width Columns of a row; anything below one counts as one.
    @param line The line to break.
    @return The rows, top to bottom. *)
val line : mode -> width:int -> Line.t -> Line.t list

(** Breaks every line of a text into rows.

    @param mode Where the lines may be broken.
    @param width Columns of a row; anything below one counts as one.
    @param text The text to break.
    @return The rows of all the lines, top to bottom. *)
val text : mode -> width:int -> Text.t -> Text.t
