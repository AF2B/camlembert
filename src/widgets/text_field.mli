(** A box where the user types one line of text.

    The text lives in a {!Line_edit.t}, which the application edits with
    {!Line_edit.handle}. The field shows the part of the text that fits,
    scrolled to keep the cursor in view, or a placeholder while the text is
    empty. Draw it with {!Frame.render_stateful_widget}, then ask for the text
    cursor with {!Frame.set_cursor} at {!cursor_position}. *)

type t

(** Builds a field.

    @param block Frame drawn around the field; none by default.
    @param style Style of the whole region; the default style if omitted.
    @param placeholder Text shown while the line is empty; nothing by default.
    @param placeholder_style Style of the placeholder; dimmed by default.
    @return The new field. *)
val make
  :  ?block:Block.t
  -> ?style:Style.t
  -> ?placeholder:string
  -> ?placeholder_style:Style.t
  -> unit
  -> t

(** Turns a field into a widget that draws it.

    @param field The field to draw.
    @return A widget that draws the field in the region it receives, scrolling
      the line it is given if the cursor would be out of view. *)
val widget : t -> Line_edit.t Stateful_widget.t

(** Where the text cursor goes when the field is drawn in a region.

    @param field The field that is drawn.
    @param edit The line being edited.
    @param area The region the field is drawn in.
    @return The cell the cursor stands on, on the first row of the field. *)
val cursor_position : t -> Line_edit.t -> area:Rect.t -> Position.t
