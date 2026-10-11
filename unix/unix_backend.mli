(** The backend for real terminals on Unix, speaking ANSI escape sequences. *)

(** Builds a backend that writes to standard output.

    Use {!with_terminal} to also take care of the terminal modes.

    @return A backend that sends cells to the terminal and reports its size. *)
val backend : unit -> Camlembert.Backend.t

(** Something the terminal can be asked to report, besides key presses. *)
type feature =
  | Mouse
  (** Mouse presses, releases, drags and wheel turns arrive as
          {!Camlembert.Event.Mouse}. *)
  | Bracketed_paste
  (** Pasted text arrives as one {!Camlembert.Event.Paste} instead of as
          key presses. *)

(** Runs a function with the whole terminal at its disposal.

    The terminal is switched to raw mode and to the alternate screen, with the
    cursor hidden, and is put back the way it was when the function returns or
    raises, including when the program is interrupted with Ctrl-C.

    @param features What else the terminal is asked to report while the
      function runs; nothing by default.
    @param run Receives the terminal to draw frames on.
    @return Whatever [run] returns.
    @raise Failure if standard input or output is not a terminal. *)
val with_terminal
  :  ?features:feature list
  -> (Camlembert.Terminal.t -> 'a)
  -> 'a
