(** Reads keyboard events and window resizes from the terminal.

    The reader waits for bytes with [select], gives them to the parser, and
    settles a lone escape byte as the Escape key when nothing follows it for a
    moment. A resize is noticed by looking at the size of the terminal
    regularly while waiting. *)

type t

(** Creates an event reader.

    @param descriptor Where the bytes come from; standard input by default.
    @param size Measures the terminal, to notice resizes; the size of the
      terminal window by default.
    @return The new reader, which takes the current size as its starting
      point. *)
val create
  :  ?descriptor:Unix.file_descr
  -> ?size:(unit -> int * int)
  -> unit
  -> t

(** Waits for the next event, for a limited time.

    @param input The reader to take the event from.
    @param timeout Most seconds to wait; zero only looks at what is there.
    @return The next event, or [None] when time ran out first.
    @raise End_of_file if the input was closed. *)
val poll : t -> timeout:float -> Camlembert.Event.t option

(** Waits for the next event, as long as it takes.

    @param input The reader to take the event from.
    @return The next event.
    @raise End_of_file if the input was closed. *)
val wait : t -> Camlembert.Event.t
