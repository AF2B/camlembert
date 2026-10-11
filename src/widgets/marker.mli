(** How finely a canvas or a chart draws.

    A marker says how many dots fit in one cell of the terminal and which
    symbol shows a given set of dots. The more dots per cell, the smoother the
    curves look. *)

type t =
  | Braille (** Two columns and four rows of dots per cell. *)
  | Half_block (** One column and two rows of dots per cell. *)
  | Block (** One dot per cell, shown as a full block. *)
  | Dot (** One dot per cell, shown as a bullet. *)

(** How many dots fit in one cell.

    @param marker The marker to inspect.
    @return The number of columns and the number of rows of dots. *)
val dots_per_cell : t -> int * int

(** The symbol that shows the painted dots of a cell.

    @param marker The marker that draws the cell.
    @param painted Tells whether the dot at a column and a row of the cell is
      painted; columns and rows count from zero inside the cell.
    @return The symbol of the cell. *)
val symbol : t -> painted:(column:int -> row:int -> bool) -> string
