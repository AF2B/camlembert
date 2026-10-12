(** The backend for real terminals on Unix, speaking ANSI escape sequences. *)

(** Builds a backend that writes to standard output.

    Use {!with_terminal} to also take care of the terminal modes. Telling where
    the cursor is works by asking the terminal and reading the answer from
    standard input, so it needs the terminal in raw mode.

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

(** Runs a function with the terminal at its disposal.

    The terminal is switched to raw mode, with the cursor hidden, and is put
    back the way it was when the function returns or raises, including when the
    program is interrupted with Ctrl-C.

    By default the program takes over the screen: it moves to the alternate
    screen, which is given back untouched at the end. With an inline viewport
    it stays on the normal screen instead, draws in some rows below the
    cursor, and leaves the last frame there, with the cursor below it, when
    the function returns. The terminal is asked where the cursor is, so it has
    to answer.

    @param features What else the terminal is asked to report while the
      function runs; nothing by default.
    @param viewport Which part of the screen frames are drawn in; the whole
      screen by default.
    @param run Receives the terminal to draw frames on.
    @return Whatever [run] returns.
    @raise Failure if standard input or output is not a terminal, or if an
      inline viewport is asked for and the terminal does not tell where its
      cursor is. *)
val with_terminal
  :  ?features:feature list
  -> ?viewport:Camlembert.Terminal.viewport
  -> (Camlembert.Terminal.t -> 'a)
  -> 'a
