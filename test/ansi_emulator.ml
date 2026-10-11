(** A tiny terminal emulator that understands the sequences Ansi produces.

    It replays output into a buffer, so tests can check that what the encoder
    sends makes a real terminal show the right cells. It is written from the
    ECMA-48 and xterm documentation, independently from the encoder. *)

module Buffer = Camlembert.Buffer
module Cell = Camlembert.Cell
module Color = Camlembert.Color
module Grapheme = Camlembert.Grapheme
module Style = Camlembert.Style

let origin = 0
let escape = '\027'
let sequence_start = '['
let sequence_introducer_length = 2
let parameter_separator = ';'
let first_final_byte = '@'
let last_final_byte = '~'
let cursor_position_final = 'H'
let select_graphic_rendition_final = 'm'

(* Select Graphic Rendition parameters. *)
let reset_code = 0
let foreground_extended_code = 38
let default_foreground_code = 39
let background_extended_code = 48
let default_background_code = 49
let palette_color_marker = 5
let true_color_marker = 2

let modifier_codes =
  let bold_code = 1 in
  let dim_code = 2 in
  let italic_code = 3 in
  let underlined_code = 4 in
  let slow_blink_code = 5 in
  let rapid_blink_code = 6 in
  let reversed_code = 7 in
  let hidden_code = 8 in
  let crossed_out_code = 9 in
  [ bold_code, Style.Bold
  ; dim_code, Style.Dim
  ; italic_code, Style.Italic
  ; underlined_code, Style.Underlined
  ; slow_blink_code, Style.Slow_blink
  ; rapid_blink_code, Style.Rapid_blink
  ; reversed_code, Style.Reversed
  ; hidden_code, Style.Hidden
  ; crossed_out_code, Style.Crossed_out
  ]
;;

type token =
  | Text of string
  | Sequence of
      { parameters : int list
      ; final : char
      }

let parameters_of body =
  if body = ""
  then []
  else List.map int_of_string (String.split_on_char parameter_separator body)
;;

let is_final_byte character =
  character >= first_final_byte && character <= last_final_byte
;;

(* Finds where the sequence whose parameters start at [position] ends. *)
let rec final_byte_position output position =
  if is_final_byte output.[position]
  then position
  else final_byte_position output (succ position)
;;

let starts_sequence output position =
  output.[position] = escape && output.[succ position] = sequence_start
;;

let tokens output =
  let length = String.length output in
  let text_between first last =
    if last > first
    then [ Text (String.sub output first (last - first)) ]
    else []
  in
  let rec scan text_start position collected =
    if position >= length
    then List.rev_append collected (text_between text_start length)
    else if starts_sequence output position
    then (
      let body_start = position + sequence_introducer_length in
      let final_position = final_byte_position output body_start in
      let body = String.sub output body_start (final_position - body_start) in
      let sequence =
        Sequence
          { parameters = parameters_of body; final = output.[final_position] }
      in
      let text_before = text_between text_start position in
      scan
        (succ final_position)
        (succ final_position)
        (sequence :: List.rev_append text_before collected))
    else scan text_start (succ position) collected
  in
  scan origin origin []
;;

(* What the terminal remembers between sequences. *)
type state =
  { column : int
  ; row : int
  ; style : Style.t
  }

let add_modifier modifier (style : Style.t) =
  { style with modifiers = Style.Modifier_set.add modifier style.modifiers }
;;

(* Applies Select Graphic Rendition parameters one after another. *)
let rec select_graphic_rendition (style : Style.t) = function
  | [] -> style
  | code :: rest when code = reset_code ->
    select_graphic_rendition Style.default rest
  | code :: marker :: index :: rest
    when code = foreground_extended_code && marker = palette_color_marker ->
    select_graphic_rendition
      { style with foreground = Some (Color.Indexed index) }
      rest
  | code :: marker :: red :: green :: blue :: rest
    when code = foreground_extended_code && marker = true_color_marker ->
    select_graphic_rendition
      { style with foreground = Some (Color.Rgb { red; green; blue }) }
      rest
  | code :: rest when code = default_foreground_code ->
    select_graphic_rendition { style with foreground = Some Color.Reset } rest
  | code :: marker :: index :: rest
    when code = background_extended_code && marker = palette_color_marker ->
    select_graphic_rendition
      { style with background = Some (Color.Indexed index) }
      rest
  | code :: marker :: red :: green :: blue :: rest
    when code = background_extended_code && marker = true_color_marker ->
    select_graphic_rendition
      { style with background = Some (Color.Rgb { red; green; blue }) }
      rest
  | code :: rest when code = default_background_code ->
    select_graphic_rendition { style with background = Some Color.Reset } rest
  | code :: rest ->
    (match List.assoc_opt code modifier_codes with
     | Some modifier ->
       select_graphic_rendition (add_modifier modifier style) rest
     | None -> failwith (Printf.sprintf "Ansi_emulator: unknown code %d" code))
;;

let write_text screen state text =
  List.fold_left
    (fun state cluster ->
       let cell = Cell.make ~style:state.style cluster in
       Buffer.apply
         screen
         [ { Buffer.column = state.column; row = state.row; cell } ];
       { state with column = state.column + Cell.width cell })
    state
    (Grapheme.clusters text)
;;

let interpret screen state = function
  | Text text -> write_text screen state text
  | Sequence { parameters = [ row; column ]; final }
    when final = cursor_position_final ->
    (* Terminals count rows and columns from one. *)
    { state with row = pred row; column = pred column }
  | Sequence { parameters; final } when final = select_graphic_rendition_final
    -> { state with style = select_graphic_rendition state.style parameters }
  | Sequence { parameters = _; final } ->
    failwith
      (Printf.sprintf "Ansi_emulator: unknown sequence ending in %c" final)
;;

let replay ~screen output =
  let first_state = { column = origin; row = origin; style = Style.default } in
  ignore (List.fold_left (interpret screen) first_state (tokens output))
;;

(* The 16 named colors are sent as palette entries, so compare them as such. *)
let normalize_color color =
  match Color.palette_index color with
  | Some index -> Color.Indexed index
  | None -> color
;;

let normalize_cell (cell : Cell.t) =
  let style = cell.style in
  Cell.make
    ~style:
      { style with
        foreground = Option.map normalize_color style.foreground
      ; background = Option.map normalize_color style.background
      }
    cell.symbol
;;
