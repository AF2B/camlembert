(** Splits a region into smaller ones, following constraints.

    Every constraint first claims the size it asks for: [Length], [Min] and
    [Max] their number of cells, [Percentage] and [Ratio] their share of the
    region, [Fill] nothing.

    The space left over is then shared among the [Fill] items by weight. When
    there is no [Fill] item, it is shared equally among the [Min] items. When
    there is neither, it stays unused at the end of the region. Rounding
    leftovers go to the last item that receives a share.

    When the items ask for more than the region has, the last items shrink
    first, down to nothing if needed. *)

type direction =
  | Horizontal (** Items sit side by side, from left to right. *)
  | Vertical (** Items are stacked, from top to bottom. *)

(** Splits a region.

    @param direction Whether the items sit side by side or stacked.
    @param constraints What each item asks for, in order.
    @param area The region to split.
    @return One region per constraint, in the same order. *)
val split
  :  direction:direction
  -> constraints:Constraint.t list
  -> Rect.t
  -> Rect.t list

(** Splits a region into side-by-side items.

    @param constraints What each item asks for, from left to right.
    @param area The region to split.
    @return One region per constraint, in the same order. *)
val horizontal : Constraint.t list -> Rect.t -> Rect.t list

(** Splits a region into stacked items.

    @param constraints What each item asks for, from top to bottom.
    @param area The region to split.
    @return One region per constraint, in the same order. *)
val vertical : Constraint.t list -> Rect.t -> Rect.t list
