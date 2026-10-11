(** How much of a line of cells an item asks for when a region is split. *)

type t =
  | Length of int (** Exactly this many cells. *)
  | Percentage of int (** This percentage of the whole region. *)
  | Ratio of
      { numerator : int
      ; denominator : int
      } (** A fraction of the whole region. *)
  | Min of int (** At least this many cells, growing when space is left. *)
  | Max of int (** At most this many cells. *)
  | Fill of int
  (** A share of the leftover space, proportional to this weight. *)
