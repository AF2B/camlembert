(* An interactive counter. Up and Down, or + and -, change the count; r resets
   it; q or Escape quits. The last event is shown at the bottom, and resizing
   the window redraws everything. *)

module Alignment = Camlembert.Alignment
module Block = Camlembert.Block
module Color = Camlembert.Color
module Constraint = Camlembert.Constraint
module Event = Camlembert.Event
module Frame = Camlembert.Frame
module Layout = Camlembert.Layout
module Line = Camlembert.Line
module Paragraph = Camlembert.Paragraph
module Span = Camlembert.Span
module Style = Camlembert.Style
module Terminal = Camlembert.Terminal

type model =
  { count : int
  ; last_event : string
  }

let initial_model = { count = Int.zero; last_event = "none yet" }
let step = Int.one
let banner_height = 3
let status_height = 3
let counter_weight = 1
let help = "Up/Down or +/- change the count, r resets it, q quits"

let title_style =
  Style.make ~foreground:Color.Yellow ~modifiers:[ Style.Bold ] ()
;;

let border_style = Style.make ~foreground:Color.Cyan ()

let positive_style =
  Style.make ~foreground:Color.Light_green ~modifiers:[ Style.Bold ] ()
;;

let negative_style =
  Style.make ~foreground:Color.Light_red ~modifiers:[ Style.Bold ] ()
;;

let hint_style =
  Style.make ~foreground:Color.Dark_gray ~modifiers:[ Style.Italic ] ()
;;

let framed title =
  Block.make
    ~borders:Block.all_sides
    ~border_type:Block.Rounded
    ~border_style
    ~title:(Line.of_string ~style:title_style title)
    ()
;;

let count_style count =
  if count > Int.zero
  then positive_style
  else if count < Int.zero
  then negative_style
  else Style.default
;;

let banner =
  Paragraph.make
    ~block:(framed " Counter ")
    ~alignment:Alignment.Center
    [ Line.of_string ~style:hint_style help ]
;;

let counter model =
  Paragraph.make
    ~alignment:Alignment.Center
    [ Line.of_string
        ~style:(count_style model.count)
        (string_of_int model.count)
    ]
;;

let status model =
  Paragraph.make
    ~block:(framed " Last event ")
    [ Line.make [ Span.make model.last_event ] ]
;;

(* Each part pairs the room it asks for with the widget that fills it. *)
let view model frame =
  let parts =
    [ Constraint.Length banner_height, Paragraph.widget banner
    ; Constraint.Fill counter_weight, Paragraph.widget (counter model)
    ; Constraint.Length status_height, Paragraph.widget (status model)
    ]
  in
  let regions = Layout.vertical (List.map fst parts) (Frame.area frame) in
  List.iter2
    (fun (_, widget) area -> Frame.render_widget frame widget ~area)
    parts
    regions
;;

let wants_to_quit = function
  | Event.Key { key = Event.Char "q" | Event.Escape; _ } -> true
  | Event.Key _ | Event.Resize _ | Event.Paste _ | Event.Mouse _ -> false
;;

let update model event =
  let model = { model with last_event = Format.asprintf "%a" Event.pp event } in
  match event with
  | Event.Key { key = Event.Up | Event.Char "+"; _ } ->
    { model with count = model.count + step }
  | Event.Key { key = Event.Down | Event.Char "-"; _ } ->
    { model with count = model.count - step }
  | Event.Key { key = Event.Char "r"; _ } ->
    { model with count = initial_model.count }
  | Event.Key _ | Event.Resize _ | Event.Paste _ | Event.Mouse _ -> model
;;

let rec run terminal events model =
  Terminal.draw terminal (view model);
  let event = Camlembert_unix.Input.wait events in
  if not (wants_to_quit event) then run terminal events (update model event)
;;

let () =
  Camlembert_unix.Unix_backend.with_terminal (fun terminal ->
    run terminal (Camlembert_unix.Input.create ()) initial_model)
;;
