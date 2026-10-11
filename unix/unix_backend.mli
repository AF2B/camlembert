(** The backend for real terminals on Unix, speaking ANSI escape sequences. *)

(** Builds a backend that writes to standard output.

    Use {!with_terminal} to also take care of the terminal modes.

    @return A backend that sends cells to the terminal and reports its size. *)
val backend : unit -> Camlembert.Backend.t

(** Runs a function with the whole terminal at its disposal.

    The terminal is switched to raw mode and to the alternate screen, with the
    cursor hidden, and is put back the way it was when the function returns or
    raises, including when the program is interrupted with Ctrl-C.

    @param run Receives the terminal to draw frames on.
    @return Whatever [run] returns.
    @raise Failure if standard input or output is not a terminal. *)
val with_terminal : (Camlembert.Terminal.t -> 'a) -> 'a
