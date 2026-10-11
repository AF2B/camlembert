(** Random value generators shared by the property-based tests. *)

module Rect = Camlembert.Rect

let max_coordinate = 50
let max_extent = 20

let rect =
  QCheck2.Gen.(
    let+ column = int_range Int.zero max_coordinate
    and+ row = int_range Int.zero max_coordinate
    and+ width = int_range Int.zero max_extent
    and+ height = int_range Int.zero max_extent in
    Rect.make ~column ~row ~width ~height)
;;

let margin = QCheck2.Gen.int_range Int.zero max_extent
let margins = QCheck2.Gen.quad margin margin margin margin
let print_rect = Format.asprintf "%a" Rect.pp
let print_margins = QCheck2.Print.(quad int int int int)
