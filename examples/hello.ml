(* A menu next to a greeting, drawn with Camlembert. Press any key to quit. *)

module Alignment = Camlembert.Alignment
module Block = Camlembert.Block
module Color = Camlembert.Color
module Constraint = Camlembert.Constraint
module Frame = Camlembert.Frame
module Layout = Camlembert.Layout
module Line = Camlembert.Line
module Paragraph = Camlembert.Paragraph
module Span = Camlembert.Span
module Style = Camlembert.Style
module Terminal = Camlembert.Terminal

let menu_width = 22
let greeting_weight = 1
let bullet = "\u{2022} "
let menu_items = [ "Widgets"; "Layout"; "Buffer"; "Backends" ]
let wide_characters = "\u{6F22}\u{5B57}\u{304B}\u{306A}"
let blank_line = Line.of_string ""

let title_style =
  Style.make ~foreground:Color.Yellow ~modifiers:[ Style.Bold ] ()
;;

let border_style = Style.make ~foreground:Color.Cyan ()
let accent_style = Style.make ~foreground:Color.Light_green ()

let hint_style =
  Style.make ~foreground:Color.Dark_gray ~modifiers:[ Style.Italic ] ()
;;

let framed ~border_type title =
  Block.make
    ~borders:Block.all_sides
    ~border_type
    ~border_style
    ~title:(Line.of_string ~style:title_style title)
    ()
;;

let menu =
  Paragraph.make
    ~block:(framed ~border_type:Block.Rounded " Menu ")
    (List.map (fun item -> Line.of_string (bullet ^ item)) menu_items)
;;

let greeting =
  Paragraph.make
    ~block:(framed ~border_type:Block.Double " Hello ")
    [ Line.make
        ~alignment:Alignment.Center
        [ Span.make ~style:title_style "Camlembert" ]
    ; blank_line
    ; Line.make
        [ Span.make "Terminal UIs for "
        ; Span.make ~style:accent_style "OCaml"
        ; Span.make ", ported from Ratatui."
        ]
    ; Line.of_string ("Wide characters line up: " ^ wide_characters)
    ; blank_line
    ; Line.make
        ~alignment:Alignment.Right
        [ Span.make ~style:hint_style "Press any key to quit" ]
    ]
;;

(* Each panel pairs the room it asks for with the widget that fills it. *)
let panels =
  [ Constraint.Length menu_width, Paragraph.widget menu
  ; Constraint.Fill greeting_weight, Paragraph.widget greeting
  ]
;;

let render_frame frame =
  let regions = Layout.horizontal (List.map fst panels) (Frame.area frame) in
  List.iter2
    (fun (_, widget) area -> Frame.render_widget frame widget ~area)
    panels
    regions
;;

let () =
  Camlembert_unix.Unix_backend.with_terminal (fun terminal ->
    Terminal.draw terminal render_frame;
    ignore (In_channel.input_char In_channel.stdin))
;;
