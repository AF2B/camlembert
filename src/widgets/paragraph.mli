(** A widget that shows text, optionally framed by a block.

    Each line of the text goes on its own row. Lines longer than the room are
    cut, and rows beyond the bottom are left out. *)

type t

(** Builds a paragraph.

    @param block Frame drawn around the text; none by default.
    @param style Style of the whole region; the default style if omitted.
    @param alignment How lines sit in the room unless they set their own;
      [Left] by default.
    @param text The lines to show.
    @return The new paragraph. *)
val make
  :  ?block:Block.t
  -> ?style:Style.t
  -> ?alignment:Alignment.t
  -> Text.t
  -> t

(** Turns a paragraph into a widget that draws it.

    @param paragraph The paragraph to draw.
    @return A widget that draws the paragraph in the region it receives. *)
val widget : t -> Widget.t
