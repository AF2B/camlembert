(* A dashboard with three tabs. Overview shows gauges and a chart that move by
   themselves, Packages a table to move through, and Logs a text to scroll.
   Tab and Shift+Tab, or Left and Right, change the tab. Up, Down, Home, End,
   Page Up and Page Down move or scroll. q or Escape quits. *)

module Alignment = Camlembert.Alignment
module Block = Camlembert.Block
module Clear = Camlembert.Clear
module Color = Camlembert.Color
module Constraint = Camlembert.Constraint
module Event = Camlembert.Event
module Frame = Camlembert.Frame
module Gauge = Camlembert.Gauge
module Layout = Camlembert.Layout
module Line = Camlembert.Line
module Paragraph = Camlembert.Paragraph
module Rect = Camlembert.Rect
module Scrollbar = Camlembert.Scrollbar
module Selection = Camlembert.Selection
module Sparkline = Camlembert.Sparkline
module Stateful_widget = Camlembert.Stateful_widget
module Style = Camlembert.Style
module Table = Camlembert.Table
module Tabs = Camlembert.Tabs
module Terminal = Camlembert.Terminal
module Text = Camlembert.Text
module Widget = Camlembert.Widget
module Wrap = Camlembert.Wrap
module Input = Camlembert_unix.Input

type tab =
  | Overview
  | Packages
  | Logs

let tabs = [ Overview; Packages; Logs ]

let title_of = function
  | Overview -> "Overview"
  | Packages -> "Packages"
  | Logs -> "Logs"
;;

let index_of tab =
  List.find_index (( = ) tab) tabs |> Option.value ~default:Int.zero
;;

let shifted_tab tab ~by =
  let count = List.length tabs in
  let shifted = (index_of tab + by + count) mod count in
  List.nth_opt tabs shifted |> Option.value ~default:tab
;;

(* Sizes of the parts of the screen. *)
let tabs_height = 3
let footer_height = 1
let border_thickness = 1
let both_sides = 2
let version_width = 10
let size_width = 9
let gauge_height = 3
let gauges_width = 34
let remaining_weight = 1

(* The numbers behind the gauges and the chart are made up. *)
let tick_seconds = 0.25
let history_size = 60
let percent_scale = 100.0
let cpu_baseline = 50
let cpu_swing = 40.0
let cpu_wave_length = 4.0
let memory_start = 30
let memory_cycle = 70
let disk_ratio = 0.72
let log_line_count = 60

let cpu_load ~tick =
  cpu_baseline
  + Float.to_int (cpu_swing *. sin (Float.of_int tick /. cpu_wave_length))
;;

let memory_load ~tick = (memory_start + tick) mod memory_cycle
let ratio_of_percent percent = Float.of_int percent /. percent_scale

let package_rows =
  List.map
    (fun (name, version, size) ->
       [ Line.of_string name
       ; Line.of_string version
       ; Line.of_string ~alignment:Alignment.Right size
       ])
    [ "alcotest", "1.9.1", "1.2 MB"
    ; "camlembert", "0.1.0", "340 KB"
    ; "dune", "3.24.2", "28.4 MB"
    ; "ocaml-lsp-server", "1.27.0", "9.8 MB"
    ; "ocamlformat", "0.29.0", "14.1 MB"
    ; "odoc", "3.2.1", "6.3 MB"
    ; "qcheck-core", "0.91", "820 KB"
    ; "uucp", "18.0.0", "21.7 MB"
    ; "uuseg", "18.0.0", "1.1 MB"
    ; "utop", "2.17.0", "3.9 MB"
    ]
;;

let log_messages =
  [ "Building the library with the strict warnings turned on."
  ; "Formatting checked: nothing to promote."
  ; "Property tests passed after one hundred generated cases each, which is a \
     long line that has to be wrapped to fit the width of the terminal."
  ; "A wide character fits in two columns: \u{6F22}\u{5B57}."
  ]
;;

let log_text =
  List.init log_line_count (fun index ->
    Printf.sprintf
      "[%03d] %s"
      index
      (List.nth log_messages (index mod List.length log_messages)))
  |> String.concat "\n"
  |> Text.of_string
;;

type model =
  { tab : tab
  ; ticks : int
  ; cpu_history : int list
  ; packages : Selection.t
  ; log_scroll : int
  }

let initial_model () =
  let packages = Selection.create () in
  Selection.select packages (Some Int.zero);
  { tab = Overview
  ; ticks = history_size
  ; cpu_history = List.init history_size (fun tick -> cpu_load ~tick)
  ; packages
  ; log_scroll = Int.zero
  }
;;

let title_style =
  Style.make ~foreground:Color.Yellow ~modifiers:[ Style.Bold ] ()
;;

let border_style = Style.make ~foreground:Color.Cyan ()

let selected_style =
  Style.make ~foreground:Color.Black ~background:Color.Cyan ()
;;

let header_style =
  Style.make ~foreground:Color.Yellow ~modifiers:[ Style.Bold ] ()
;;

let hint_style =
  Style.make ~foreground:Color.Dark_gray ~modifiers:[ Style.Italic ] ()
;;

let framed title =
  Block.make
    ~borders:Block.all_sides
    ~border_type:Block.Rounded
    ~border_style
    ~title:(Line.of_string ~style:title_style (" " ^ title ^ " "))
    ()
;;

let gauge title ~percent ~color =
  Gauge.widget
    (Gauge.make
       ~block:(framed title)
       ~gauge_style:(Style.make ~foreground:color ())
       ~ratio:(ratio_of_percent percent)
       ())
;;

let overview model : Widget.t =
  let gauges =
    Widget.split
      ~direction:Layout.Vertical
      [ ( Constraint.Length gauge_height
        , gauge "CPU" ~percent:(cpu_load ~tick:model.ticks) ~color:Color.Green )
      ; ( Constraint.Length gauge_height
        , gauge
            "Memory"
            ~percent:(memory_load ~tick:model.ticks)
            ~color:Color.Yellow )
      ; ( Constraint.Length gauge_height
        , gauge
            "Disk"
            ~percent:(Float.to_int (disk_ratio *. percent_scale))
            ~color:Color.Cyan )
      ; Constraint.Fill remaining_weight, Clear.widget
      ]
  in
  let chart =
    Sparkline.widget
      (Sparkline.make
         ~block:(framed "CPU history")
         ~style:(Style.make ~foreground:Color.Green ())
         ~maximum:(Float.to_int percent_scale)
         model.cpu_history)
  in
  Widget.split
    ~direction:Layout.Horizontal
    [ Constraint.Length gauges_width, gauges
    ; Constraint.Fill remaining_weight, chart
    ]
;;

let packages model : Widget.t =
  let table =
    Table.make
      ~block:(framed "Packages")
      ~header:
        [ Line.of_string "Package"
        ; Line.of_string "Version"
        ; Line.of_string ~alignment:Alignment.Right "Size"
        ]
      ~header_style
      ~highlight_symbol:"\u{25B6} "
      ~highlight_style:selected_style
      ~widths:
        [ Constraint.Fill remaining_weight
        ; Constraint.Length version_width
        ; Constraint.Length size_width
        ]
      package_rows
  in
  Stateful_widget.with_state (Table.widget table) model.packages
;;

let log_paragraph model =
  Paragraph.make
    ~block:(framed "Logs")
    ~wrap:Wrap.Word
    ~scroll:model.log_scroll
    log_text
;;

(* How many rows of the log are visible, and how wide they are, on a screen. *)
let log_viewport screen =
  let chrome = tabs_height + footer_height + (both_sides * border_thickness) in
  ( screen.Rect.width - (both_sides * border_thickness)
  , Int.max Int.one (screen.Rect.height - chrome) )
;;

let logs model : Widget.t =
  fun area buffer ->
  let paragraph = log_paragraph model in
  let width = area.Rect.width - (both_sides * border_thickness) in
  let height = area.Rect.height - (both_sides * border_thickness) in
  Paragraph.widget paragraph area buffer;
  Scrollbar.widget
    (Scrollbar.make
       ~thumb_style:border_style
       ~side:Block.Right
       ~content_length:(Paragraph.line_count paragraph ~width)
       ~viewport_length:height
       ~position:model.log_scroll
       ())
    (Rect.shrink
       area
       ~top:border_thickness
       ~bottom:border_thickness
       ~left:Int.zero
       ~right:Int.zero)
    buffer
;;

let help_for = function
  | Overview -> "The numbers move by themselves"
  | Packages -> "Up, Down, Home, End move"
  | Logs -> "Up, Down, PgUp, PgDn scroll"
;;

let footer model =
  Paragraph.widget
    (Paragraph.make
       ~alignment:Alignment.Center
       [ Line.of_string
           ~style:hint_style
           (help_for model.tab ^ " | Tab switches tab | q quits")
       ])
;;

let tab_bar model =
  Tabs.widget
    (Tabs.make
       ~block:
         (Block.make
            ~borders:Block.all_sides
            ~border_type:Block.Rounded
            ~border_style
            ())
       ~highlight_style:selected_style
       ~selected:(index_of model.tab)
       (List.map (fun tab -> Line.of_string (title_of tab)) tabs))
;;

let view model frame =
  let body =
    match model.tab with
    | Overview -> overview model
    | Packages -> packages model
    | Logs -> logs model
  in
  Frame.render_widget
    frame
    (Widget.split
       ~direction:Layout.Vertical
       [ Constraint.Length tabs_height, tab_bar model
       ; Constraint.Fill remaining_weight, body
       ; Constraint.Length footer_height, footer model
       ])
    ~area:(Frame.area frame)
;;

let tick model =
  let ticks = succ model.ticks in
  let history = model.cpu_history @ [ cpu_load ~tick:ticks ] in
  let skipped = Int.max Int.zero (List.length history - history_size) in
  { model with
    ticks
  ; cpu_history = List.filteri (fun index _ -> index >= skipped) history
  }
;;

let scroll_logs model key ~screen =
  let width, height = log_viewport screen in
  let line_count = Paragraph.line_count (log_paragraph model) ~width in
  let furthest = Int.max Int.zero (line_count - height) in
  let scrolled =
    match key with
    | Event.Up -> model.log_scroll - Int.one
    | Event.Down -> model.log_scroll + Int.one
    | Event.Page_up -> model.log_scroll - height
    | Event.Page_down -> model.log_scroll + height
    | Event.Home -> Int.zero
    | Event.End -> furthest
    | _ -> model.log_scroll
  in
  { model with log_scroll = Int.min furthest (Int.max Int.zero scrolled) }
;;

let move_in_packages model key =
  let item_count = List.length package_rows in
  (match key with
   | Event.Up -> Selection.select_previous model.packages ~item_count
   | Event.Down -> Selection.select_next model.packages ~item_count
   | Event.Home -> Selection.select model.packages (Some Int.zero)
   | Event.End -> Selection.select model.packages (Some (pred item_count))
   | _ -> ());
  model
;;

let update model event ~screen =
  match event with
  | Event.Key { key = Event.Tab | Event.Right; _ } ->
    { model with tab = shifted_tab model.tab ~by:Int.one }
  | Event.Key { key = Event.Backtab | Event.Left; _ } ->
    { model with tab = shifted_tab model.tab ~by:(Int.neg Int.one) }
  | Event.Key { key; _ } ->
    (match model.tab with
     | Overview -> model
     | Packages -> move_in_packages model key
     | Logs -> scroll_logs model key ~screen)
  | Event.Resize _ | Event.Paste _ -> model
;;

let wants_to_quit = function
  | Event.Key { key = Event.Char "q" | Event.Escape; _ } -> true
  | Event.Key _ | Event.Resize _ | Event.Paste _ -> false
;;

let rec run terminal events model =
  Terminal.draw terminal (view model);
  match Input.poll events ~timeout:tick_seconds with
  | None -> run terminal events (tick model)
  | Some event when wants_to_quit event -> ()
  | Some event ->
    run terminal events (update model event ~screen:(Terminal.area terminal))
;;

let () =
  Camlembert_unix.Unix_backend.with_terminal (fun terminal ->
    run terminal (Input.create ()) (initial_model ()))
;;
