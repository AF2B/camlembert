(** A list of items, one per row, with one of them selected.

    The list draws with a {!Selection.t}, which tells the selected row and
    which rows are scrolled out of view. Drawing scrolls the list so that the
    selected row stays visible. Draw it with {!Frame.render_stateful_widget}. *)

type t

(** Builds a list.

    @param block Frame drawn around the list; none by default.
    @param style Style of the whole region; the default style if omitted.
    @param highlight_style Style layered over the whole selected row; the
      default style if omitted.
    @param highlight_symbol Text shown before the selected row. Every row
      leaves room for it while something is selected. Nothing by default.
    @param items The rows, from top to bottom. Each is one line.
    @return The new list. *)
val make
  :  ?block:Block.t
  -> ?style:Style.t
  -> ?highlight_style:Style.t
  -> ?highlight_symbol:string
  -> Line.t list
  -> t

(** Turns a list into a widget that draws it.

    @param list The list to draw.
    @return A widget that draws the list in the region it receives, updating
      the selection state it is given. *)
val widget : t -> Selection.t Stateful_widget.t

(** Finds the item under a cell, for reacting to a click.

    The scrolling of the selection state is taken into account, as drawn.

    @param list The list that is drawn.
    @param selection The state the list is drawn with.
    @param area The region the list is drawn in.
    @param position The cell to look at.
    @return The position of the item, counted from zero, or [None] when the
      cell is not over an item. *)
val index_at : t -> Selection.t -> area:Rect.t -> Position.t -> int option
