(** Terminal colors. *)

type t =
  | Reset (** The default color of the terminal. *)
  | Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | Gray
  | Dark_gray
  | Light_red
  | Light_green
  | Light_yellow
  | Light_blue
  | Light_magenta
  | Light_cyan
  | White
  | Indexed of int (** An entry of the 256-color palette. *)
  | Rgb of
      { red : int
      ; green : int
      ; blue : int
      } (** A 24-bit true color. *)

(** The 16 named colors, in the order of the standard ANSI palette. *)
val ansi_palette : t list

(** Finds the position of a color in the 256-color palette.

    @param color The color to look up.
    @return
      The palette position, or [None] for [Reset] and [Rgb], which are not
      part of the palette. *)
val palette_index : t -> int option

(** Structural equality.

    @param first One color.
    @param second The other color.
    @return [true] when both colors are the same. *)
val equal : t -> t -> bool

(** Pretty-printer, handy for test failures.

    @param formatter Where to print.
    @param color The color to print. *)
val pp : Format.formatter -> t -> unit
