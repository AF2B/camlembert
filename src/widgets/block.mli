(** A frame around other content, with optional borders, a title, a footer and
    padding.

    A block draws its borders along the edges of the region it is given, and
    {!inner} tells where the content that sits inside goes. A title takes the
    top row of the block and a footer takes the bottom row. They are drawn
    over the borders, so they also take the border style unless their spans
    set their own colors. Padding keeps the content away from the borders. *)

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

(** Cells kept free between the borders and the content, on each side. *)
type padding =
  { left : int
  ; right : int
  ; top : int
  ; bottom : int
  }

type t

(** The four sides of a block, for a border all around. *)
val all_sides : side list

(** Padding that keeps nothing free. *)
val no_padding : padding

(** The same padding on every side.

    @param size Cells kept free on each side.
    @return The padding. *)
val uniform_padding : int -> padding

(** Builds a block.

    @param borders Sides that get a border; none by default.
    @param border_type Look of the border lines; [Plain] by default.
    @param border_style Style of the border lines; the default style if omitted.
    @param style Style of the whole region, content included.
    @param title Line drawn on the top row of the block.
    @param title_alignment Where the title sits on the top row; [Left] by
      default.
    @param footer Line drawn on the bottom row of the block.
    @param footer_alignment Where the footer sits on the bottom row; [Left] by
      default.
    @param padding Cells kept free inside the borders; none by default.
    @return The new block.
    @raise Invalid_argument if a side of the padding is negative. *)
val make
  :  ?borders:side list
  -> ?border_type:border_type
  -> ?border_style:Style.t
  -> ?style:Style.t
  -> ?title:Line.t
  -> ?title_alignment:Alignment.t
  -> ?footer:Line.t
  -> ?footer_alignment:Alignment.t
  -> ?padding:padding
  -> unit
  -> t

(** Finds the region inside the borders and the padding, where the content
    goes.

    A title also takes the top row, and a footer the bottom row, even without
    a border on that side.

    @param block The block that frames the content.
    @param area The region the block is drawn in.
    @return The region left for the content, empty if the block fills it all. *)
val inner : t -> Rect.t -> Rect.t

(** Turns a block into a widget that draws it.

    @param block The block to draw.
    @return A widget that draws the block in the region it receives. *)
val widget : t -> Widget.t
