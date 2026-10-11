(** A frame around other content, with optional borders and a title.

    A block draws its borders along the edges of the region it is given, and
    {!inner} tells where the content that sits inside goes. A title takes the
    top row of the block. It is drawn over the top border, so it also takes the
    border style unless its spans set their own colors. *)

type side =
  | Top
  | Bottom
  | Left
  | Right

type border_type =
  | Plain (** Thin straight lines. *)
  | Rounded (** Thin lines with rounded corners. *)
  | Double (** Double lines. *)
  | Thick (** Thick lines. *)

type t

(** The four sides of a block, for a border all around. *)
val all_sides : side list

(** Builds a block.

    @param borders Sides that get a border; none by default.
    @param border_type Look of the border lines; [Plain] by default.
    @param border_style Style of the border lines; the default style if omitted.
    @param style Style of the whole region, content included.
    @param title Line drawn on the top row of the block.
    @param title_alignment Where the title sits on the top row; [Left] by
      default.
    @return The new block. *)
val make
  :  ?borders:side list
  -> ?border_type:border_type
  -> ?border_style:Style.t
  -> ?style:Style.t
  -> ?title:Line.t
  -> ?title_alignment:Alignment.t
  -> unit
  -> t

(** Finds the region inside the borders, where the content goes.

    A title also takes the top row, even without a top border.

    @param block The block that frames the content.
    @param area The region the block is drawn in.
    @return The region left for the content, empty if the block fills it all. *)
val inner : t -> Rect.t -> Rect.t

(** Turns a block into a widget that draws it.

    @param block The block to draw.
    @return A widget that draws the block in the region it receives. *)
val widget : t -> Widget.t
