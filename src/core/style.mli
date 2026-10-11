(** The appearance of a terminal cell: colors and text effects. *)

type modifier =
  | Bold
  | Dim
  | Italic
  | Underlined
  | Slow_blink
  | Rapid_blink
  | Reversed
  | Hidden
  | Crossed_out

(** A set of text effects. *)
module Modifier_set : Set.S with type elt = modifier

type t =
  { foreground : Color.t option
    (** Text color, or [None] to leave it unchanged. *)
  ; background : Color.t option
    (** Background color, or [None] to leave it unchanged. *)
  ; modifiers : Modifier_set.t (** Text effects switched on. *)
  }

(** A style that changes nothing. *)
val default : t

(** Builds a style.

    @param foreground Text color.
    @param background Background color.
    @param modifiers Text effects to switch on; the order does not matter.
    @return The new style. *)
val make
  :  ?foreground:Color.t
  -> ?background:Color.t
  -> ?modifiers:modifier list
  -> unit
  -> t

(** Layers a style over another one.

    Colors of the overlay win when present, otherwise the base ones are kept.
    Text effects of both styles are combined.

    @param base The style underneath.
    @param overlay The style applied on top.
    @return The combined style. *)
val patch : t -> t -> t

(** Structural equality.

    @param first One style.
    @param second The other style.
    @return [true] when colors and text effects are the same. *)
val equal : t -> t -> bool

(** Pretty-printer, handy for test failures.

    @param formatter Where to print.
    @param style The style to print. *)
val pp : Format.formatter -> t -> unit
