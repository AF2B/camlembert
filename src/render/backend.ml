(** The bridge between the library and a real, or fake, terminal.

    The library works out which cells changed; a backend is the only part that
    turns them into output. Any record of these four functions is a backend. *)

type t =
  { size : unit -> Rect.t
    (** Region currently available on the screen, starting at the origin. *)
  ; draw : Buffer.change list -> unit (** Shows the changed cells. *)
  ; clear : unit -> unit (** Blanks the whole screen. *)
  ; flush : unit -> unit (** Makes everything drawn so far visible. *)
  }
