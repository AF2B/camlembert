(** Several lines of text. *)

type t = Line.t list

(** Builds a text from a string, one line per line feed.

    A trailing line feed yields a last empty line.

    @param style How the text is drawn; defaults to the default style.
    @param content The UTF-8 text to split.
    @return The lines of the text, top to bottom. *)
val of_string : ?style:Style.t -> string -> t
