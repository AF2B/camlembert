(* Bytes received but not turned into events yet. *)
type t = { pending : string }

let empty = { pending = "" }

(* Bytes with a meaning of their own. *)
let escape = '\027'
let line_feed = '\n'
let carriage_return = '\r'
let tab = '\t'
let backspace = '\b'
let delete = '\127'
let null = '\000'
let space = " "

(* Typing a letter while holding control sends a byte counted from the first
   control character. Control with a few symbols works the same way. *)
let first_control_letter = '\001'
let last_control_letter = '\026'
let first_control_symbol = '\028'
let last_control_symbol = '\031'
let control_symbol_offset = 64
let lowercase_a = 'a'

(* Pieces of the escape sequences, from the xterm control sequences. *)
let sequence_start = '['
let single_shift_start = 'O'
let introducer_length = 2
let single_byte = 1
let parameter_separator = ';'
let first_intermediate_byte = ' '
let last_parameter_byte = '?'
let first_final_byte = '@'
let last_final_byte = '~'

(* Final bytes of the cursor key sequences. *)
let up_final = 'A'
let down_final = 'B'
let right_final = 'C'
let left_final = 'D'
let home_final = 'H'
let end_final = 'F'
let backtab_final = 'Z'
let tilde_final = '~'

(* Final bytes of F1 to F4 in the single shift sequences. *)
let f1_final = 'P'
let f2_final = 'Q'
let f3_final = 'R'
let f4_final = 'S'

(* Numbers that name a key in the sequences ending with a tilde. *)
let home_code = 1
let insert_code = 2
let delete_code = 3
let end_code = 4
let page_up_code = 5
let page_down_code = 6
let home_alternate_code = 7
let end_alternate_code = 8
let f1_code = 11
let f2_code = 12
let f3_code = 13
let f4_code = 14
let f5_code = 15
let f6_code = 17
let f7_code = 18
let f8_code = 19
let f9_code = 20
let f10_code = 21
let f11_code = 23
let f12_code = 24

(* A bracketed paste starts with one sequence and ends with another. *)
let paste_start_code = 200
let paste_end_code = 201

let paste_marker code =
  Printf.sprintf "%c%c%d%c" escape sequence_start code tilde_final
;;

let paste_start_marker = paste_marker paste_start_code
let paste_end_marker = paste_marker paste_end_code

(* A modifier number is one more than the sum of the modifiers held. *)
let modifier_code_offset = 1
let shift_bit = 1
let alt_bit = 2
let control_bit = 4

let cursor_keys =
  [ up_final, Event.Up
  ; down_final, Event.Down
  ; right_final, Event.Right
  ; left_final, Event.Left
  ; home_final, Event.Home
  ; end_final, Event.End
  ]
;;

let editing_keys =
  [ home_code, Event.Home
  ; insert_code, Event.Insert
  ; delete_code, Event.Delete
  ; end_code, Event.End
  ; page_up_code, Event.Page_up
  ; page_down_code, Event.Page_down
  ; home_alternate_code, Event.Home
  ; end_alternate_code, Event.End
  ]
;;

(* F1 is the first one, F2 the second one, and so on. *)
let function_key_finals = [ f1_final; f2_final; f3_final; f4_final ]

let function_key_codes =
  [ f1_code
  ; f2_code
  ; f3_code
  ; f4_code
  ; f5_code
  ; f6_code
  ; f7_code
  ; f8_code
  ; f9_code
  ; f10_code
  ; f11_code
  ; f12_code
  ]
;;

(* What reading the bytes at some position gives. *)
type decoded =
  | Pressed of Event.key * Event.modifiers * int
  | Reported of Event.t * int
  | Ignored of int
  | Incomplete

let pressed ?(modifiers = Event.no_modifiers) key length =
  Pressed (key, modifiers, length)
;;

let held_control = { Event.no_modifiers with control = true }

let control_key character =
  pressed
    ~modifiers:held_control
    (Event.Char (String.make single_byte character))
    single_byte
;;

let function_key_at index = Event.Function (succ index)

let modifiers_of_code code =
  let bits = code - modifier_code_offset in
  { Event.shift = bits land shift_bit <> Int.zero
  ; alt = bits land alt_bit <> Int.zero
  ; control = bits land control_bit <> Int.zero
  }
;;

let decode_text input position =
  let decoded = String.get_utf_8_uchar input position in
  let length = Uchar.utf_decode_length decoded in
  if Uchar.utf_decode_is_valid decoded
  then pressed (Event.Char (String.sub input position length)) length
  else if position + length = String.length input
  then Incomplete
  else Ignored length
;;

let decode_plain input position =
  let lead = input.[position] in
  if lead = carriage_return || lead = line_feed
  then pressed Event.Enter single_byte
  else if lead = tab
  then pressed Event.Tab single_byte
  else if lead = backspace || lead = delete
  then pressed Event.Backspace single_byte
  else if lead = null
  then pressed ~modifiers:held_control (Event.Char space) single_byte
  else if lead >= first_control_letter && lead <= last_control_letter
  then
    control_key
      (Char.chr
         (Char.code lead
          - Char.code first_control_letter
          + Char.code lowercase_a))
  else if lead >= first_control_symbol && lead <= last_control_symbol
  then control_key (Char.chr (Char.code lead + control_symbol_offset))
  else decode_text input position
;;

(* Alt sends an escape byte before the key. *)
let decode_alt input position =
  match decode_plain input (succ position) with
  | Pressed (key, modifiers, length) ->
    Pressed (key, { modifiers with Event.alt = true }, succ length)
  | Reported (event, length) -> Reported (event, succ length)
  | Ignored length -> Ignored (succ length)
  | Incomplete -> Incomplete
;;

type sequence_end =
  | Final of int
  | Unfinished
  | Malformed

(* Looks for the byte that ends a sequence. Before it come only parameter and
   intermediate bytes, which are all the bytes from space to question mark. *)
let rec find_final input index =
  if index >= String.length input
  then Unfinished
  else (
    let byte = input.[index] in
    if byte >= first_final_byte && byte <= last_final_byte
    then Final index
    else if byte >= first_intermediate_byte && byte <= last_parameter_byte
    then find_final input (succ index)
    else Malformed)
;;

let numbers_of body =
  if body = ""
  then Some []
  else (
    let parts =
      List.map int_of_string_opt (String.split_on_char parameter_separator body)
    in
    if List.for_all Option.is_some parts
    then Some (List.filter_map Fun.id parts)
    else None)
;;

let modifiers_after = function
  | _ :: code :: _ -> modifiers_of_code code
  | _ -> Event.no_modifiers
;;

let tilde_key code =
  match List.assoc_opt code editing_keys with
  | Some _ as key -> key
  | None ->
    List.find_index (Int.equal code) function_key_codes
    |> Option.map function_key_at
;;

let single_shift_key final =
  match List.assoc_opt final cursor_keys with
  | Some _ as key -> key
  | None ->
    List.find_index (Char.equal final) function_key_finals
    |> Option.map function_key_at
;;

let csi_key numbers final =
  let modifiers = modifiers_after numbers in
  if final = tilde_final
  then (
    match numbers with
    | code :: _ -> tilde_key code |> Option.map (fun key -> key, modifiers)
    | [] -> None)
  else if final = backtab_final
  then Some (Event.Backtab, Event.no_modifiers)
  else
    List.assoc_opt final cursor_keys |> Option.map (fun key -> key, modifiers)
;;

let matches_at input marker ~position =
  let rec compare_from offset =
    offset >= String.length marker
    || (input.[position + offset] = marker.[offset]
        && compare_from (succ offset))
  in
  position + String.length marker <= String.length input
  && compare_from Int.zero
;;

let rec find_marker input marker ~from =
  if from + String.length marker > String.length input
  then None
  else if matches_at input marker ~position:from
  then Some from
  else find_marker input marker ~from:(succ from)
;;

(* The text of a bracketed paste, up to the sequence that ends it. *)
let decode_paste input ~sequence_start:position ~content_start =
  match find_marker input paste_end_marker ~from:content_start with
  | None -> Incomplete
  | Some content_end ->
    let pasted = String.sub input content_start (content_end - content_start) in
    Reported
      ( Event.Paste pasted
      , content_end + String.length paste_end_marker - position )
;;

let decode_sequence input position =
  let body_start = position + introducer_length in
  match find_final input body_start with
  | Unfinished -> Incomplete
  | Malformed -> Ignored introducer_length
  | Final final_index ->
    let length = succ final_index - position in
    let body = String.sub input body_start (final_index - body_start) in
    if matches_at input paste_start_marker ~position
    then
      decode_paste
        input
        ~sequence_start:position
        ~content_start:(succ final_index)
    else (
      match
        Option.bind (numbers_of body) (fun numbers ->
          csi_key numbers input.[final_index])
      with
      | Some (key, modifiers) -> Pressed (key, modifiers, length)
      | None -> Ignored length)
;;

let decode_single_shift input position =
  let length = introducer_length + single_byte in
  if String.length input - position < length
  then Incomplete
  else (
    match single_shift_key input.[position + introducer_length] with
    | Some key -> pressed key length
    | None -> Ignored length)
;;

let decode_escape input position =
  if String.length input - position = single_byte
  then Incomplete
  else (
    let next = input.[succ position] in
    if next = sequence_start
    then decode_sequence input position
    else if next = single_shift_start
    then decode_single_shift input position
    else if next = escape
    then pressed Event.Escape single_byte
    else decode_alt input position)
;;

(* Once no more bytes are coming, an unfinished sequence is given up: a lone
   escape byte is the Escape key, anything else is dropped. *)
let decode ~final input position =
  let is_escape = input.[position] = escape in
  let decoded =
    if is_escape
    then decode_escape input position
    else decode_plain input position
  in
  match decoded with
  | Incomplete when final && is_escape -> pressed Event.Escape single_byte
  | Incomplete when final -> Ignored single_byte
  | _ -> decoded
;;

let parse ~final input =
  let rec loop position events =
    if position >= String.length input
    then List.rev events, ""
    else (
      match decode ~final input position with
      | Incomplete ->
        ( List.rev events
        , String.sub input position (String.length input - position) )
      | Ignored length -> loop (position + length) events
      | Reported (event, length) -> loop (position + length) (event :: events)
      | Pressed (key, modifiers, length) ->
        loop (position + length) (Event.Key { key; modifiers } :: events))
  in
  loop Int.zero []
;;

(* While a paste is being received, a pause says nothing: the end marker is
   still on its way. *)
let has_pending parser =
  parser.pending <> ""
  && not (matches_at parser.pending paste_start_marker ~position:Int.zero)
;;

let feed parser bytes =
  let events, pending = parse ~final:false (parser.pending ^ bytes) in
  { pending }, events
;;

let flush parser =
  let events, pending = parse ~final:true parser.pending in
  { pending }, events
;;
