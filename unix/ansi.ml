module Cell = Camlembert.Cell
module Color = Camlembert.Color
module Style = Camlembert.Style

let escape = "\027"
let control_sequence_introducer = escape ^ "["
let parameter_separator = ";"
let private_mode_marker = "?"

(* Final bytes: the character that tells which sequence it is. *)
let cursor_position_final = "H"
let erase_display_final = "J"
let select_graphic_rendition_final = "m"
let set_mode_final = "h"
let reset_mode_final = "l"

(* Numbers used by the sequences above. *)
let erase_entire_display = 2
let alternate_screen_mode = 1049
let cursor_visibility_mode = 25
let bracketed_paste_mode = 2004

(* Select Graphic Rendition parameters, from ECMA-48 and xterm. *)
let reset_code = 0
let bold_code = 1
let dim_code = 2
let italic_code = 3
let underlined_code = 4
let slow_blink_code = 5
let rapid_blink_code = 6
let reversed_code = 7
let hidden_code = 8
let crossed_out_code = 9
let foreground_extended_code = 38
let default_foreground_code = 39
let background_extended_code = 48
let default_background_code = 49
let palette_color_marker = 5
let true_color_marker = 2

let sequence ~parameters final =
  control_sequence_introducer
  ^ String.concat parameter_separator (List.map string_of_int parameters)
  ^ final
;;

let private_mode ~mode final =
  control_sequence_introducer ^ private_mode_marker ^ string_of_int mode ^ final
;;

let enter_alternate_screen =
  private_mode ~mode:alternate_screen_mode set_mode_final
;;

let leave_alternate_screen =
  private_mode ~mode:alternate_screen_mode reset_mode_final
;;

let show_cursor = private_mode ~mode:cursor_visibility_mode set_mode_final
let hide_cursor = private_mode ~mode:cursor_visibility_mode reset_mode_final

let enable_bracketed_paste =
  private_mode ~mode:bracketed_paste_mode set_mode_final
;;

let disable_bracketed_paste =
  private_mode ~mode:bracketed_paste_mode reset_mode_final
;;

let clear_screen =
  sequence ~parameters:[ erase_entire_display ] erase_display_final
  ^ sequence ~parameters:[] cursor_position_final
;;

let reset_style =
  sequence ~parameters:[ reset_code ] select_graphic_rendition_final
;;

let move_cursor ~column ~row =
  sequence ~parameters:[ succ row; succ column ] cursor_position_final
;;

(* The cursor is moved before it is shown, so it never flashes elsewhere. *)
let encode_cursor = function
  | Some { Camlembert.Position.column; row } ->
    move_cursor ~column ~row ^ show_cursor
  | None -> hide_cursor
;;

let modifier_code = function
  | Style.Bold -> bold_code
  | Style.Dim -> dim_code
  | Style.Italic -> italic_code
  | Style.Underlined -> underlined_code
  | Style.Slow_blink -> slow_blink_code
  | Style.Rapid_blink -> rapid_blink_code
  | Style.Reversed -> reversed_code
  | Style.Hidden -> hidden_code
  | Style.Crossed_out -> crossed_out_code
;;

let palette_codes ~extended_code color =
  Color.palette_index color
  |> Option.map (fun index -> [ extended_code; palette_color_marker; index ])
  |> Option.value ~default:[]
;;

let color_codes ~extended_code ~default_code = function
  | Color.Reset -> [ default_code ]
  | Color.Rgb { red; green; blue } ->
    [ extended_code; true_color_marker; red; green; blue ]
  | color -> palette_codes ~extended_code color
;;

let style_codes (style : Style.t) =
  let foreground =
    Option.map
      (color_codes
         ~extended_code:foreground_extended_code
         ~default_code:default_foreground_code)
      style.foreground
  in
  let background =
    Option.map
      (color_codes
         ~extended_code:background_extended_code
         ~default_code:default_background_code)
      style.background
  in
  List.map modifier_code (Style.Modifier_set.elements style.modifiers)
  @ Option.value foreground ~default:[]
  @ Option.value background ~default:[]
;;

let select_style style =
  match style_codes style with
  | [] -> ""
  | codes -> sequence ~parameters:codes select_graphic_rendition_final
;;

(* What has been written so far, and what the terminal is showing. Pieces of
   output are kept newest first. *)
type encoding =
  { pieces : string list
  ; next_position : (int * int) option
  ; style : Style.t
  }

let first_encoding =
  { pieces = []; next_position = None; style = Style.default }
;;

let encode_change encoding ({ column; row; cell } : Camlembert.Buffer.change) =
  let move =
    if encoding.next_position = Some (column, row)
    then []
    else [ move_cursor ~column ~row ]
  in
  let restyle =
    if Style.equal encoding.style cell.Cell.style
    then []
    else if Style.equal encoding.style Style.default
    then [ select_style cell.Cell.style ]
    else [ reset_style; select_style cell.Cell.style ]
  in
  { pieces =
      List.rev_append (move @ restyle @ [ cell.Cell.symbol ]) encoding.pieces
  ; next_position = Some (column + Cell.width cell, row)
  ; style = cell.Cell.style
  }
;;

let encode_changes changes =
  match changes with
  | [] -> ""
  | _ ->
    let encoding = List.fold_left encode_change first_encoding changes in
    String.concat "" (List.rev (reset_style :: encoding.pieces))
;;
