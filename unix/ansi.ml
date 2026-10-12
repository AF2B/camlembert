module Cell = Camlembert.Cell
module Color = Camlembert.Color
module Style = Camlembert.Style

let escape_character = '\027'
let escape = String.make Int.one escape_character
let control_sequence_introducer = escape ^ "["
let parameter_separator = ";"
let private_mode_marker = "?"
let line_feed = '\n'

(* Final bytes: the character that tells which sequence it is. *)
let cursor_position_final = "H"
let erase_display_final = "J"
let device_status_report_final = "n"
let cursor_report_final = 'R'
let select_graphic_rendition_final = "m"
let set_mode_final = "h"
let reset_mode_final = "l"

(* Numbers used by the sequences above. *)
let erase_from_cursor = 0
let erase_entire_display = 2
let cursor_report_request = 6
let alternate_screen_mode = 1049
let cursor_visibility_mode = 25
let bracketed_paste_mode = 2004

(* The mouse is reported in three layers: presses and releases, moves while a
   button is held, and the extended format that gives big coordinates. *)
let mouse_press_mode = 1000
let mouse_drag_mode = 1002
let mouse_extended_mode = 1006

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
let mouse_modes = [ mouse_press_mode; mouse_drag_mode; mouse_extended_mode ]

let enable_mouse =
  String.concat
    ""
    (List.map (fun mode -> private_mode ~mode set_mode_final) mouse_modes)
;;

let disable_mouse =
  String.concat
    ""
    (List.map
       (fun mode -> private_mode ~mode reset_mode_final)
       (List.rev mouse_modes))
;;

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

let clear_from ~column ~row =
  move_cursor ~column ~row
  ^ sequence ~parameters:[ erase_from_cursor ] erase_display_final
;;

let scroll_up ~rows ~last_row =
  if rows <= Int.zero
  then ""
  else move_cursor ~column:Int.zero ~row:last_row ^ String.make rows line_feed
;;

let request_cursor_position =
  sequence ~parameters:[ cursor_report_request ] device_status_report_final
;;

let is_digit character = character >= '0' && character <= '9'

let character_at text index =
  if index < String.length text then Some text.[index] else None
;;

(* Reads the run of digits that starts at [start]: the number and the position
   just after the run. *)
let number_at text start =
  let rec run_end index =
    if Option.fold ~none:false ~some:is_digit (character_at text index)
    then run_end (succ index)
    else index
  in
  let stop = run_end start in
  if stop = start
  then None
  else
    int_of_string_opt (String.sub text start (stop - start))
    |> Option.map (fun number -> number, stop)
;;

let has_text_at text ~start expected =
  let stop = start + String.length expected in
  stop <= String.length text
  && String.equal (String.sub text start (String.length expected)) expected
;;

(* A report reads [ESC \[ row ; column R], counted from one. *)
let report_at text ~start =
  let row_start = start + String.length control_sequence_introducer in
  if not (has_text_at text ~start control_sequence_introducer)
  then None
  else
    Option.bind (number_at text row_start) (fun (row, after_row) ->
      if not (has_text_at text ~start:after_row parameter_separator)
      then None
      else
        Option.bind
          (number_at text (after_row + String.length parameter_separator))
          (fun (column, after_column) ->
             if character_at text after_column = Some cursor_report_final
             then
               Some
                 { Camlembert.Position.column = Int.max Int.zero (pred column)
                 ; row = Int.max Int.zero (pred row)
                 }
             else None))
;;

let find_cursor_report text =
  let rec search_from index =
    match String.index_from_opt text index escape_character with
    | None -> None
    | Some start ->
      (match report_at text ~start with
       | Some _ as report -> report
       | None -> search_from (succ start))
  in
  search_from Int.zero
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
