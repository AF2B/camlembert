(** What happens in the terminal that an application may react to. *)

type key =
  | Char of string (** A character, written as one UTF-8 character. *)
  | Enter
  | Tab
  | Backtab (** Tab with shift held. *)
  | Backspace
  | Escape
  | Delete
  | Insert
  | Up
  | Down
  | Left
  | Right
  | Home
  | End
  | Page_up
  | Page_down
  | Function of int (** A function key: [Function 5] is F5. *)

(** The modifier keys held while a key was pressed. *)
type modifiers =
  { shift : bool
  ; control : bool
  ; alt : bool
  }

type t =
  | Key of
      { key : key
      ; modifiers : modifiers
      } (** A key was pressed. *)
  | Resize of
      { width : int
      ; height : int
      } (** The terminal window got a new size, in cells. *)

(** No modifier key held. *)
val no_modifiers : modifiers

(** Builds the event of a key press.

    @param modifiers The modifier keys held; none by default.
    @param key The key that was pressed.
    @return The event. *)
val key_press : ?modifiers:modifiers -> key -> t

(** Structural equality.

    @param first One event.
    @param second The other event.
    @return [true] when both events are the same. *)
val equal : t -> t -> bool

(** Pretty-printer, handy for test failures and for showing events.

    @param formatter Where to print.
    @param event The event to print. *)
val pp : Format.formatter -> t -> unit
