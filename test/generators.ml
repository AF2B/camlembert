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

let max_text_length = 12
let first_printable_ascii = ' '
let last_printable_ascii = '~'
let latin_letter = Uchar.of_char 'a'
let line_feed = Uchar.of_char '\n'
let combining_acute = Uchar.of_int 0x0301
let zero_width_joiner = Uchar.of_int 0x200D
let cjk_ideograph = Uchar.of_int 0x6F22
let man_emoji = Uchar.of_int 0x1F468
let regional_indicator = Uchar.of_int 0x1F1E7

let interesting_scalars =
  [ latin_letter
  ; line_feed
  ; combining_acute
  ; zero_width_joiner
  ; cjk_ideograph
  ; man_emoji
  ; regional_indicator
  ]
;;

let printable_ascii_text =
  QCheck2.Gen.(
    string_size
      ~gen:(char_range first_printable_ascii last_printable_ascii)
      (int_range Int.zero max_text_length))
;;

let mixed_text =
  QCheck2.Gen.(
    let+ scalars =
      list_size
        (int_range Int.zero max_text_length)
        (oneof_list interesting_scalars)
    in
    let buffer = Stdlib.Buffer.create max_text_length in
    List.iter (Stdlib.Buffer.add_utf_8_uchar buffer) scalars;
    Stdlib.Buffer.contents buffer)
;;

let print_rect = Format.asprintf "%a" Rect.pp
let print_margins = QCheck2.Print.(quad int int int int)
let print_style = Format.asprintf "%a" Style.pp
