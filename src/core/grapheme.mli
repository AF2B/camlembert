(** Text measured in terminal columns.

    A terminal shows one user-perceived character, called a grapheme cluster,
    per group of columns. ASCII letters take one column, CJK characters and
    most emoji take two, and combining marks take none. *)

(** Splits a text into its grapheme clusters.

    @param text The UTF-8 text to split.
    @return
      The clusters in reading order, or an empty list for an empty text. *)
val clusters : string -> string list

(** Measures how many terminal columns a text occupies.

    @param text The UTF-8 text to measure.
    @return
      The summed width of its clusters. A cluster is at most two columns wide
      and control characters take none. *)
val width : string -> int
