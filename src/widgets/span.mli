(** A piece of text drawn with a single style. *)

type t =
  { content : string (** The UTF-8 text of the span. *)
  ; style : Style.t (** How the text is drawn. *)
  }

(** Builds a span.

    @param style How the text is drawn; defaults to the default style.
    @param content The UTF-8 text of the span.
    @return The new span. *)
val make : ?style:Style.t -> string -> t

(** Number of terminal columns the span takes.

    @param span The span to measure.
    @return The width of its text. *)
val width : t -> int
