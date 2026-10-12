(** A cell of the terminal screen, found by its column and its row. *)

type t =
  { column : int (** Column of the cell, counted from zero on the left. *)
  ; row : int (** Row of the cell, counted from zero at the top. *)
  }

(** The top-left cell of the screen. *)
let origin = { column = Int.zero; row = Int.zero }
