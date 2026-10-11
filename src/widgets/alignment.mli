(** How content sits inside the room it has. *)

type t =
  | Left (** Content starts at the left edge. *)
  | Center (** Content is centered; an odd leftover column goes to the right. *)
  | Right (** Content ends at the right edge. *)

(** Counts the empty columns to leave before the content.

    @param alignment How the content sits in the room.
    @param available Columns the room has.
    @param content Columns the content takes.
    @return
      The empty columns before the content, or zero when the content does not
      fit. *)
val offset : t -> available:int -> content:int -> int
