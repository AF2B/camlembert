(** The terminal attached to the process: its size and its input mode. *)

(** The terminal settings found before they were changed. *)
type saved_mode

(** Measures the terminal window.

    @return The number of columns and the number of rows.
    @raise Failure if standard output is not a terminal. *)
val size : unit -> int * int

(** Switches the terminal to raw mode.

    Key presses are no longer echoed, nor held until a line is complete, and
    output is no longer rewritten. Interrupt signals still work.

    @return The previous settings, to give back to {!restore_mode}.
    @raise Unix.Unix_error if standard input is not a terminal. *)
val enter_raw_mode : unit -> saved_mode

(** Puts the terminal back the way it was.

    @param saved_mode The settings returned by {!enter_raw_mode}. *)
val restore_mode : saved_mode -> unit

(** Waits for bytes to arrive, for a limited time.

    @param descriptor Where the bytes come from, usually standard input.
    @param seconds Most seconds to wait; zero only looks at what is there.
    @return The bytes that arrived, or [None] when time ran out, or when the
      wait was interrupted.
    @raise End_of_file if the descriptor was closed. *)
val read_within : descriptor:Unix.file_descr -> seconds:float -> string option
