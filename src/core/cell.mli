(** One cell of the terminal screen: a symbol and the style it is drawn with. *)

type t =
  { symbol : string (** The grapheme cluster shown in the cell. *)
  ; style : Style.t (** How the symbol is drawn. *)
  }

(** A blank cell: a space with the default style. *)
val empty : t

(** Builds a cell.

    @param style How the symbol is drawn; defaults to the default style.
    @param symbol The grapheme cluster to show.
    @return The new cell. *)
val make : ?style:Style.t -> string -> t

(** Replaces the symbol of a cell, keeping its style.

    @param cell The cell to change.
    @param symbol The grapheme cluster to show.
    @return The updated cell. *)
val with_symbol : t -> string -> t

(** Layers a style over the one the cell already has.

    @param cell The cell to change.
    @param style The style applied on top.
    @return The updated cell. *)
val patch_style : t -> Style.t -> t

(** Number of terminal columns the symbol of a cell takes.

    @param cell The cell to measure.
    @return The width of its symbol. *)
val width : t -> int

(** Structural equality.

    @param first One cell.
    @param second The other cell.
    @return [true] when symbols and styles are the same. *)
val equal : t -> t -> bool

(** Pretty-printer, handy for test failures.

    @param formatter Where to print.
    @param cell The cell to print. *)
val pp : Format.formatter -> t -> unit
