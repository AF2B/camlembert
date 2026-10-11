(** A bar along one edge of a region that shows how far its content is scrolled.

    The bar has a track and, on it, a thumb. The thumb is as long as the
    visible part of the content is relative to the whole content, and it sits
    where the visible part is. The bar does not scroll anything: it only shows
    numbers that the application keeps. *)

type t

(** Builds a scrollbar.

    @param track_style Style of the track; the default style if omitted.
    @param thumb_style Style of the thumb; the default style if omitted.
    @param track_symbol Symbol repeated along the track; a thin line by
      default.
    @param thumb_symbol Symbol repeated along the thumb; a full block by
      default.
    @param side Edge of the region the bar is drawn on. The right and left
      edges give a vertical bar, the top and bottom edges a horizontal one.
    @param content_length How many rows, or columns, the whole content has.
    @param viewport_length How many of them are visible at once.
    @param position How many are scrolled out of view before the first visible
      one; beyond the last useful position counts as the last one.
    @return The new scrollbar.
    @raise Invalid_argument if a length or the position is negative. *)
val make
  :  ?track_style:Style.t
  -> ?thumb_style:Style.t
  -> ?track_symbol:string
  -> ?thumb_symbol:string
  -> side:Block.side
  -> content_length:int
  -> viewport_length:int
  -> position:int
  -> unit
  -> t

(** Turns a scrollbar into a widget that draws it.

    @param scrollbar The scrollbar to draw.
    @return A widget that draws the scrollbar along an edge of the region it
      receives. *)
val widget : t -> Widget.t
