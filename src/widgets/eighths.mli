(** Fractions of a cell, in eighths, drawn with block characters.

    Charts use them to be more precise than a whole cell. *)

(** How many eighths make a cell. *)
val per_cell : int

(** The block that fills a cell from the bottom up to some eighths.

    @param eighths How many eighths are filled, from zero to eight; values
      outside of that range are brought into it.
    @return A blank for zero, a full block for eight. *)
val lower_block : int -> string

(** The block that fills a cell from the left up to some eighths.

    @param eighths How many eighths are filled, from zero to eight; values
      outside of that range are brought into it.
    @return A blank for zero, a full block for eight. *)
val left_block : int -> string

(** How many eighths a fraction of a run of cells fills.

    @param fraction How much of the run is filled, from zero to one; values
      outside of that range are brought into it.
    @param cells How many cells the run has.
    @return The eighths filled, rounded to the nearest one. *)
val filled : fraction:float -> cells:int -> int
