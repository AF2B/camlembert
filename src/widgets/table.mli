(** Rows of cells lined up in columns, with an optional header.

    The width of each column comes from a layout constraint, and a few cells
    of spacing separate the columns. Every cell is one line, cut when it is
    longer than its column. A table is drawn with a {!Selection.t}, like
    {!Item_list}: the selected row is highlighted, the rows scroll to keep it
    in view, and the header always stays on top. Draw it with
    {!Frame.render_stateful_widget}. *)

type t

(** Builds a table.

    @param block Frame drawn around the table; none by default.
    @param style Style of the whole region; the default style if omitted.
    @param header Cells of the first row, which does not scroll; no header by
      default.
    @param header_style Style layered over the header row; the default style
      if omitted.
    @param highlight_style Style layered over the whole selected row; the
      default style if omitted.
    @param highlight_symbol Text shown before the selected row. Every row
      leaves room for it while something is selected. Nothing by default.
    @param column_spacing Cells between two columns; one by default.
    @param widths What each column asks for, from left to right.
    @param rows The rows, from top to bottom, each made of one line per cell.
      A row with more cells than columns loses the extra ones.
    @return The new table. *)
val make
  :  ?block:Block.t
  -> ?style:Style.t
  -> ?header:Line.t list
  -> ?header_style:Style.t
  -> ?highlight_style:Style.t
  -> ?highlight_symbol:string
  -> ?column_spacing:int
  -> widths:Constraint.t list
  -> Line.t list list
  -> t

(** Turns a table into a widget that draws it.

    @param table The table to draw.
    @return A widget that draws the table in the region it receives, updating
      the selection state it is given. *)
val widget : t -> Selection.t Stateful_widget.t

(** Finds the row under a cell, for reacting to a click.

    The scrolling of the selection state is taken into account, as drawn. The
    header is not a row.

    @param table The table that is drawn.
    @param selection The state the table is drawn with.
    @param area The region the table is drawn in.
    @param position The cell to look at.
    @return The position of the row, counted from zero, or [None] when the
      cell is not over a row. *)
val index_at : t -> Selection.t -> area:Rect.t -> Position.t -> int option
