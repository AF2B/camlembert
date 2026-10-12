(* An installer that shows its progress in a few rows below the prompt, without
   taking over the screen. Each package that finishes is printed above the
   progress bars and stays in the terminal history, and the last frame is
   still there when the program ends. q or Escape stops it. *)

module Color = Camlembert.Color
module Constraint = Camlembert.Constraint
module Event = Camlembert.Event
module Frame = Camlembert.Frame
module Layout = Camlembert.Layout
module Line = Camlembert.Line
module Line_gauge = Camlembert.Line_gauge
module Paragraph = Camlembert.Paragraph
module Span = Camlembert.Span
module Style = Camlembert.Style
module Terminal = Camlembert.Terminal
module Widget = Camlembert.Widget
module Input = Camlembert_unix.Input
module Unix_backend = Camlembert_unix.Unix_backend

type package =
  { name : string
  ; version : string
  ; ticks : int
  }

(* The packages and how long each one takes are made up. *)
let packages =
  [ { name = "ocaml-base-compiler"; version = "5.5.1"; ticks = 30 }
  ; { name = "dune"; version = "3.24.2"; ticks = 18 }
  ; { name = "uucp"; version = "18.0.0"; ticks = 24 }
  ; { name = "uuseg"; version = "18.0.0"; ticks = 10 }
  ; { name = "alcotest"; version = "1.9.1"; ticks = 14 }
  ; { name = "qcheck-core"; version = "0.91"; ticks = 12 }
  ; { name = "ocamlformat"; version = "0.29.0"; ticks = 22 }
  ; { name = "odoc"; version = "3.2.1"; ticks = 20 }
  ]
;;

let package_count = List.length packages
let tick_seconds = 0.08
let viewport_height = 4
let row_height = 1
let label_width = 28
let first_package = 0
let no_ticks = 0
let finished_mark = "\u{2714} "

let sum_of_ticks selected =
  List.fold_left (fun total package -> total + package.ticks) no_ticks selected
;;

let total_ticks = sum_of_ticks packages

let title_style =
  Style.make ~foreground:Color.Yellow ~modifiers:[ Style.Bold ] ()
;;

let finished_style = Style.make ~foreground:Color.Light_green ()
let package_style = Style.make ~foreground:Color.Cyan ()
let total_style = Style.make ~foreground:Color.Magenta ()
let remaining_style = Style.make ~foreground:Color.Dark_gray ()

let hint_style =
  Style.make ~foreground:Color.Dark_gray ~modifiers:[ Style.Italic ] ()
;;

type model =
  { finished : int
  ; ticks_done : int
  }

let initial_model = { finished = first_package; ticks_done = no_ticks }
let current_package model = List.nth_opt packages model.finished
let is_complete model = model.finished >= package_count

let installed_ticks model =
  sum_of_ticks (List.filteri (fun index _ -> index < model.finished) packages)
  + model.ticks_done
;;

let ratio_of ~done_ ~out_of = Float.of_int done_ /. Float.of_int out_of
let padded text = Printf.sprintf "%-*s" label_width text
let describe package = package.name ^ " " ^ package.version

let progress_row model : Widget.t =
  match current_package model with
  | None ->
    Paragraph.widget
      (Paragraph.make
         [ Line.of_string ~style:finished_style "Everything is installed" ])
  | Some package ->
    Line_gauge.widget
      (Line_gauge.make
         ~label:(Line.of_string (padded (describe package)))
         ~filled_style:package_style
         ~unfilled_style:remaining_style
         ~ratio:(ratio_of ~done_:model.ticks_done ~out_of:package.ticks)
         ())
;;

let total_row model : Widget.t =
  Line_gauge.widget
    (Line_gauge.make
       ~label:(Line.of_string (padded "Total"))
       ~filled_style:total_style
       ~unfilled_style:remaining_style
       ~ratio:(ratio_of ~done_:(installed_ticks model) ~out_of:total_ticks)
       ())
;;

let text_row line : Widget.t = Paragraph.widget (Paragraph.make [ line ])

(* Once everything is installed there is nothing left to stop. *)
let hint model =
  if is_complete model
  then Line.of_string ""
  else Line.of_string ~style:hint_style "q or Escape stops"
;;

let view model frame =
  let rows =
    [ ( Constraint.Length row_height
      , text_row
          (Line.of_string
             ~style:title_style
             (Printf.sprintf "Installing %d packages" package_count)) )
    ; Constraint.Length row_height, progress_row model
    ; Constraint.Length row_height, total_row model
    ; Constraint.Length row_height, text_row (hint model)
    ]
  in
  Frame.render_widget
    frame
    (Widget.split ~direction:Layout.Vertical rows)
    ~area:(Frame.area frame)
;;

(* The package that was being installed when the model moved on. *)
let tick model =
  match current_package model with
  | None -> model
  | Some package ->
    let ticks_done = succ model.ticks_done in
    if ticks_done >= package.ticks
    then { finished = succ model.finished; ticks_done = no_ticks }
    else { model with ticks_done }
;;

let print_finished terminal package =
  Terminal.insert_before terminal ~height:row_height (fun frame ->
    Frame.render_widget
      frame
      (text_row
         (Line.make
            [ Span.make ~style:finished_style finished_mark
            ; Span.make (describe package)
            ]))
      ~area:(Frame.area frame))
;;

let wants_to_stop = function
  | Event.Key { key = Event.Char "q" | Event.Escape; _ } -> true
  | Event.Key _ | Event.Resize _ | Event.Paste _ | Event.Mouse _ -> false
;;

let rec run terminal events model =
  Terminal.draw terminal (view model);
  if not (is_complete model)
  then (
    match Input.poll events ~timeout:tick_seconds with
    | Some event when wants_to_stop event -> ()
    | Some _ -> run terminal events model
    | None ->
      let next = tick model in
      if next.finished > model.finished
      then Option.iter (print_finished terminal) (current_package model);
      run terminal events next)
;;

let () =
  try
    Unix_backend.with_terminal
      ~viewport:(Terminal.Inline viewport_height)
      (fun terminal -> run terminal (Input.create ()) initial_model)
  with
  | Sys.Break -> ()
;;
