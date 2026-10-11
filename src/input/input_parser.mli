(** Turns the bytes a terminal sends into events.

    Terminals send a key as a plain character or as an escape sequence, and a
    sequence may arrive in pieces. The parser keeps the bytes it cannot decide
    on yet and finishes them when more bytes arrive. A lone escape byte is
    ambiguous: it is the Escape key, or the start of a sequence. The reader
    settles it by calling {!flush} when nothing else shows up in time.

    Understood: printable and UTF-8 characters, Enter, Tab, Backspace, Control
    and Alt combinations, cursor and editing keys with their modifiers,
    function keys F1 to F12, Shift+Tab, and pasted text between the sequences
    that mark it. Anything else is ignored. *)

type t

(** A parser that has not received any byte yet. *)
val empty : t

(** Gives bytes to the parser.

    @param parser The parser that received the earlier bytes.
    @param bytes The bytes that just arrived, in any amount.
    @return The parser to use next, and the events completed so far. *)
val feed : t -> string -> t * Event.t list

(** Tells whether some bytes are waiting for the rest of their sequence.

    The bytes of a paste that is still arriving do not count: a pause says
    nothing then, since the sequence that ends the paste is on its way.

    @param parser The parser to inspect.
    @return [true] when settling the pending bytes with {!flush} makes sense. *)
val has_pending : t -> bool

(** Gives up waiting for the rest of a sequence.

    A lone escape byte becomes the Escape key. Other unfinished sequences are
    read as the plain characters they are made of.

    @param parser The parser to settle.
    @return An empty parser, and the events the pending bytes stand for. *)
val flush : t -> t * Event.t list
