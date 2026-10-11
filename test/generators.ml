(** Random value generators shared by the property-based tests. *)

module Alignment = Camlembert.Alignment
module Block = Camlembert.Block
module Color = Camlembert.Color
module Constraint = Camlembert.Constraint
module Layout = Camlembert.Layout
module Line = Camlembert.Line
module Marker = Camlembert.Marker
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

let screen_width = 8
let screen_height = 4
let max_edit_count = 10

let screen =
  Rect.make
    ~column:Int.zero
    ~row:Int.zero
    ~width:screen_width
    ~height:screen_height
;;

(** A single-character write at a position of the test screen. *)
type edit =
  { column : int
  ; row : int
  ; symbol : string
  ; style : Style.t
  }

let edit =
  QCheck2.Gen.(
    let+ column = int_range Int.zero (pred screen_width)
    and+ row = int_range Int.zero (pred screen_height)
    and+ character = char_range first_printable_ascii last_printable_ascii
    and+ style = style in
    { column; row; symbol = String.make Int.one character; style })
;;

let edits = QCheck2.Gen.(list_size (int_range Int.zero max_edit_count) edit)

let print_edit edit =
  Printf.sprintf "(%d, %d, %S)" edit.column edit.row edit.symbol
;;

let print_edits = QCheck2.Print.list print_edit
let max_constraint_size = 30
let max_percentage = 120
let max_ratio_term = 6
let max_constraint_count = 6

let layout_constraint =
  QCheck2.Gen.(
    let size = int_range Int.zero max_constraint_size in
    oneof
      [ map (fun length -> Constraint.Length length) size
      ; map
          (fun percentage -> Constraint.Percentage percentage)
          (int_range Int.zero max_percentage)
      ; (let+ numerator = int_range Int.zero max_ratio_term
         and+ denominator = int_range Int.zero max_ratio_term in
         Constraint.Ratio { numerator; denominator })
      ; map (fun minimum -> Constraint.Min minimum) size
      ; map (fun maximum -> Constraint.Max maximum) size
      ; map (fun weight -> Constraint.Fill weight) size
      ])
;;

let layout_constraints =
  QCheck2.Gen.(
    list_size (int_range Int.zero max_constraint_count) layout_constraint)
;;

let layout_direction =
  QCheck2.Gen.oneof_list [ Layout.Horizontal; Layout.Vertical ]
;;

let print_constraint = function
  | Constraint.Length length -> Printf.sprintf "Length %d" length
  | Constraint.Percentage percentage ->
    Printf.sprintf "Percentage %d" percentage
  | Constraint.Ratio { numerator; denominator } ->
    Printf.sprintf "Ratio %d/%d" numerator denominator
  | Constraint.Min minimum -> Printf.sprintf "Min %d" minimum
  | Constraint.Max maximum -> Printf.sprintf "Max %d" maximum
  | Constraint.Fill weight -> Printf.sprintf "Fill %d" weight
;;

let print_constraints = QCheck2.Print.list print_constraint
let wrap_alphabet = [ "a"; "b"; "c"; " "; " "; "\u{6F22}" ]
let max_wrapped_length = 30
let narrowest_wide_row = 2
let widest_row = 12

(** Text made of letters, spaces and a wide character, to be wrapped. *)
let wrappable_text =
  QCheck2.Gen.(
    let+ pieces =
      list_size
        (int_range Int.zero max_wrapped_length)
        (oneof_list wrap_alphabet)
    in
    String.concat "" pieces)
;;

(** Row widths wide enough to hold any single character of [wrappable_text]. *)
let row_width = QCheck2.Gen.int_range narrowest_wide_row widest_row

let max_line_count = 4
let alignment = QCheck2.Gen.oneof_list Alignment.[ Left; Center; Right ]

let text_lines =
  QCheck2.Gen.(
    list_size (int_range Int.zero max_line_count) printable_ascii_text)
;;

let print_alignment = function
  | Alignment.Left -> "Left"
  | Alignment.Center -> "Center"
  | Alignment.Right -> "Right"
;;

let block_sides =
  QCheck2.Gen.(
    list_size
      (int_range Int.zero (List.length Block.all_sides))
      (oneof_list Block.all_sides))
;;

let block_border_type =
  QCheck2.Gen.oneof_list Block.[ Plain; Rounded; Double; Thick ]
;;

(** The parameters of a random block: sides, border type and optional title. *)
let block_parameters =
  QCheck2.Gen.(
    triple block_sides block_border_type (option printable_ascii_text))
;;

let block_of_parameters (borders, border_type, title_text) =
  Block.make
    ~borders
    ~border_type
    ?title:(Option.map (fun text -> Line.of_string text) title_text)
    ()
;;

let print_block_parameters (borders, border_type, title_text) =
  let side_name = function
    | Block.Top -> "Top"
    | Block.Bottom -> "Bottom"
    | Block.Left -> "Left"
    | Block.Right -> "Right"
  in
  let border_type_name = function
    | Block.Plain -> "Plain"
    | Block.Rounded -> "Rounded"
    | Block.Double -> "Double"
    | Block.Thick -> "Thick"
  in
  Printf.sprintf
    "borders=[%s] border_type=%s title=%s"
    (String.concat "; " (List.map side_name borders))
    (border_type_name border_type)
    (QCheck2.Print.(option string) title_text)
;;

let ratio = QCheck2.Gen.float_range 0.0 1.0
let print_rect = Format.asprintf "%a" Rect.pp
let print_margins = QCheck2.Print.(quad int int int int)
let print_style = Format.asprintf "%a" Style.pp
let markers = [ Marker.Braille; Marker.Half_block; Marker.Block; Marker.Dot ]
let marker = QCheck2.Gen.oneof_list markers

let print_marker = function
  | Marker.Braille -> "Braille"
  | Marker.Half_block -> "Half_block"
  | Marker.Block -> "Block"
  | Marker.Dot -> "Dot"
;;
