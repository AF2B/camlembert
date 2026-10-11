(** Random value generators shared by the property-based tests. *)

module Color = Camlembert.Color
module Rect = Camlembert.Rect
module Style = Camlembert.Style

let max_coordinate = 50
let max_extent = 20
let max_color_channel = 255
let max_modifier_count = 5

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
let color_channel = QCheck2.Gen.int_range Int.zero max_color_channel

let color =
  QCheck2.Gen.(
    oneof
      [ oneof_list (Color.Reset :: Color.ansi_palette)
      ; map (fun index -> Color.Indexed index) color_channel
      ; (let+ red = color_channel
         and+ green = color_channel
         and+ blue = color_channel in
         Color.Rgb { red; green; blue })
      ])
;;

let all_modifiers =
  Style.
    [ Bold
    ; Dim
    ; Italic
    ; Underlined
    ; Slow_blink
    ; Rapid_blink
    ; Reversed
    ; Hidden
    ; Crossed_out
    ]
;;

let style =
  QCheck2.Gen.(
    let+ foreground = option color
    and+ background = option color
    and+ modifiers =
      list_size
        (int_range Int.zero max_modifier_count)
        (oneof_list all_modifiers)
    in
    Style.make ?foreground ?background ~modifiers ())
;;

let print_rect = Format.asprintf "%a" Rect.pp
let print_margins = QCheck2.Print.(quad int int int int)
let print_style = Format.asprintf "%a" Style.pp
