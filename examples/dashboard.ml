(* A dashboard with four tabs. Overview shows gauges, a sparkline and bars that
   move by themselves. Charts plots two series and draws a radar on a canvas,
   both moving by themselves. Packages has a filter to type in and a table to
   move through. Logs is a text to scroll. Everything works with the keyboard
   and with the mouse: click a tab or a row, turn the wheel, paste into the
   filter. Tab and Shift+Tab change the tab, Escape asks to quit. *)

module Alignment = Camlembert.Alignment
module Bar_chart = Camlembert.Bar_chart
module Block = Camlembert.Block
module Canvas = Camlembert.Canvas
module Chart = Camlembert.Chart
module Clear = Camlembert.Clear
module Color = Camlembert.Color
module Constraint = Camlembert.Constraint
module Event = Camlembert.Event
module Frame = Camlembert.Frame
module Gauge = Camlembert.Gauge
module Layout = Camlembert.Layout
module Line = Camlembert.Line
module Line_edit = Camlembert.Line_edit
module Line_gauge = Camlembert.Line_gauge
module Marker = Camlembert.Marker
module Paragraph = Camlembert.Paragraph
module Rect = Camlembert.Rect
module Scrollbar = Camlembert.Scrollbar
module Selection = Camlembert.Selection
module Sparkline = Camlembert.Sparkline
module Style = Camlembert.Style
module Table = Camlembert.Table
module Tabs = Camlembert.Tabs
module Terminal = Camlembert.Terminal
module Text = Camlembert.Text
module Text_field = Camlembert.Text_field
module Widget = Camlembert.Widget
module Wrap = Camlembert.Wrap
module Input = Camlembert_unix.Input
module Unix_backend = Camlembert_unix.Unix_backend

type tab =
  | Overview
  | Charts
  | Packages
  | Logs

let tabs = [ Overview; Charts; Packages; Logs ]

let title_of = function
  | Overview -> "Overview"
  | Charts -> "Charts"
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
let filter_height = 3
let border_thickness = 1
let gauge_height = 3
let gauges_width = 34
let bars_height = 9
let version_width = 10
let size_width = 9
let remaining_weight = 1
let chart_weight = 2
let chart_gap = 1
let log_side_padding = 1
let dialog_width = 34
let dialog_height = 5

(* The numbers behind the gauges, the chart and the bars are made up. *)
let tick_seconds = 0.25
let history_size = 60
let percent_scale = 100.0
let cpu_baseline = 50
let cpu_swing = 40.0
let cpu_wave_length = 4.0
let memory_start = 30
let memory_cycle = 70
let disk_ratio = 0.72
let requests_baseline = 20
let requests_cycle = 60
let requests_per_tick = 3
let requests_per_hour = 17
let network_step = 7
let network_cycle = 101
let hour_count = 6
let log_line_count = 60
let wheel_rows = 3

(* The radar on the Charts tab: rings, a cross, blips and a sweeping beam, with
   the beam fading behind its head. Distances are fractions of the radius. *)
let radar_marker = Marker.Braille
let center = 0.0
let radar_radius = 1.0
let ring_radii = [ 0.33; 0.66; radar_radius ]
let blips = [ 0.5, 0.3; -0.4, 0.55; -0.2, -0.6; 0.7, -0.35 ]
let sweep_step = 0.2

let beam_trail =
  [ 0.0, Color.Light_green
  ; 0.12, Color.Green
  ; 0.24, Color.Green
  ; 0.36, Color.Dark_gray
  ]
;;

let percent_labels = [ "0"; "50"; "100" ]

let cpu_load ~tick =
  cpu_baseline
  + Float.to_int (cpu_swing *. sin (Float.of_int tick /. cpu_wave_length))
;;

let memory_load ~tick = (memory_start + tick) mod memory_cycle
let network_load ~tick = tick * network_step mod network_cycle
let ratio_of_percent percent = Float.of_int percent /. percent_scale

let requests ~tick ~hour =
  requests_baseline
  + (((tick * requests_per_tick) + (hour * requests_per_hour))
     mod requests_cycle)
;;

let all_packages =
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

let contains ~sub text =
  let rec from index =
    index + String.length sub <= String.length text
    && (String.equal (String.sub text index (String.length sub)) sub
        || from (succ index))
  in
  from Int.zero
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
  ; filter : Line_edit.t
  ; packages : Selection.t
  ; log_scroll : int
  ; confirming_quit : bool
  }

let initial_model () =
  let packages = Selection.create () in
  Selection.select packages (Some Int.zero);
  { tab = Overview
  ; ticks = history_size
  ; cpu_history = List.init history_size (fun tick -> cpu_load ~tick)
  ; filter = Line_edit.create ()
  ; packages
  ; log_scroll = Int.zero
  ; confirming_quit = false
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

let framed ?footer ?(padding = Block.no_padding) title =
  Block.make
    ~borders:Block.all_sides
    ~border_type:Block.Rounded
    ~border_style
    ~title:(Line.of_string ~style:title_style (" " ^ title ^ " "))
    ?footer
    ~footer_alignment:Alignment.Right
    ~padding
    ()
;;

(* The parts of the screen, from top to bottom. *)
type regions =
  { tab_bar : Rect.t
  ; body : Rect.t
  ; footer : Rect.t
  }

let regions screen =
  match
    Layout.vertical
      [ Constraint.Length tabs_height
      ; Constraint.Fill remaining_weight
      ; Constraint.Length footer_height
      ]
      screen
  with
  | [ tab_bar; body; footer ] -> { tab_bar; body; footer }
  | _ -> invalid_arg "Dashboard.regions: expected three regions"
;;

(* The filter and the table on the Packages tab, from top to bottom. *)
let packages_regions body =
  match
    Layout.vertical
      [ Constraint.Length filter_height; Constraint.Fill remaining_weight ]
      body
  with
  | [ filter; table ] -> filter, table
  | _ -> invalid_arg "Dashboard.packages_regions: expected two regions"
;;

let tab_bar model =
  Tabs.make
    ~block:
      (Block.make
         ~borders:Block.all_sides
         ~border_type:Block.Rounded
         ~border_style
         ())
    ~highlight_style:selected_style
    ~selected:(index_of model.tab)
    (List.map (fun tab -> Line.of_string (title_of tab)) tabs)
;;

let filter_field =
  Text_field.make
    ~block:(framed "Filter")
    ~placeholder:"type to filter the packages"
    ()
;;

let visible_packages model =
  let wanted = String.lowercase_ascii (Line_edit.text model.filter) in
  List.filter
    (fun (name, _, _) -> contains ~sub:wanted (String.lowercase_ascii name))
    all_packages
;;

let package_count model =
  Line.of_string
    ~style:hint_style
    (Printf.sprintf
       " %d of %d "
       (List.length (visible_packages model))
       (List.length all_packages))
;;

let packages_table model =
  Table.make
    ~block:(framed ~footer:(package_count model) "Packages")
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
    (List.map
       (fun (name, version, size) ->
          [ Line.of_string name
          ; Line.of_string version
          ; Line.of_string ~alignment:Alignment.Right size
          ])
       (visible_packages model))
;;

let gauge title ~percent ~color =
  Gauge.widget
    (Gauge.make
       ~block:(framed title)
       ~gauge_style:(Style.make ~foreground:color ())
       ~ratio:(ratio_of_percent percent)
       ())
;;

let line_gauge title ~percent ~color =
  Line_gauge.widget
    (Line_gauge.make
       ~block:(framed title)
       ~filled_style:(Style.make ~foreground:color ())
       ~unfilled_style:(Style.make ~foreground:Color.Dark_gray ())
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
      ; ( Constraint.Length gauge_height
        , line_gauge
            "Network"
            ~percent:(network_load ~tick:model.ticks)
            ~color:Color.Magenta )
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
  let bars =
    Bar_chart.widget
      (Bar_chart.make
         ~block:(framed "Requests per hour")
         ~bar_style:(Style.make ~foreground:Color.Magenta ())
         (List.init hour_count (fun hour ->
            { Bar_chart.label = Printf.sprintf "h%d" hour
            ; value = requests ~tick:model.ticks ~hour
            })))
  in
  Widget.split
    ~direction:Layout.Horizontal
    [ Constraint.Length gauges_width, gauges
    ; ( Constraint.Fill remaining_weight
      , Widget.split
          ~direction:Layout.Vertical
          [ Constraint.Fill remaining_weight, chart
          ; Constraint.Length bars_height, bars
          ] )
    ]
;;

let axis_start = 0.0
let halving_divisor = 2.0
let history_seconds = Float.of_int history_size *. tick_seconds

let history_labels =
  [ Printf.sprintf "-%.0f s" history_seconds
  ; Printf.sprintf "-%.1f s" (history_seconds /. halving_divisor)
  ; "now"
  ]
;;

let history_chart model : Widget.t =
  let points percents =
    List.mapi
      (fun index percent -> Float.of_int index, Float.of_int percent)
      percents
  in
  let memory_history =
    List.init history_size (fun index ->
      memory_load ~tick:(model.ticks - history_size + succ index))
  in
  let series ~name ~color percents =
    Chart.dataset
      ~graph_type:Chart.Connected
      ~style:(Style.make ~foreground:color ())
      ~name
      (points percents)
  in
  Chart.widget
    (Chart.make
       ~block:(framed "CPU and memory")
       ~x_axis:
         (Chart.axis
            ~title:"time"
            ~labels:history_labels
            ~bounds:(axis_start, Float.of_int (pred history_size))
            ())
       ~y_axis:
         (Chart.axis
            ~title:"%"
            ~labels:percent_labels
            ~bounds:(axis_start, percent_scale)
            ())
       [ series ~name:"CPU" ~color:Color.Green model.cpu_history
       ; series ~name:"Memory" ~color:Color.Yellow memory_history
       ])
;;

(* Bounds that make a unit as long along both axes, so that a circle looks
   round: the shorter side of the region goes from -1 to 1. *)
let square_bounds marker ~width ~height =
  let dots_per_column, dots_per_row = Marker.dots_per_cell marker in
  let dots_wide = width * dots_per_column in
  let dots_tall = height * dots_per_row in
  let shorter = Float.of_int (Int.min dots_wide dots_tall) in
  let half_range dots = Float.of_int dots /. shorter in
  ( (Float.neg (half_range dots_wide), half_range dots_wide)
  , (Float.neg (half_range dots_tall), half_range dots_tall) )
;;

let radar_shapes model =
  let ring_style = Style.make ~foreground:Color.Dark_gray () in
  let blip_style = Style.make ~foreground:Color.Yellow () in
  let angle = Float.of_int model.ticks *. sweep_step in
  let ring radius =
    Canvas.Circle { x = center; y = center; radius; style = ring_style }
  in
  let spoke (x, y) =
    Canvas.Segment
      { x1 = Float.neg x; y1 = Float.neg y; x2 = x; y2 = y; style = ring_style }
  in
  let beam (gap, color) =
    Canvas.Segment
      { x1 = center
      ; y1 = center
      ; x2 = radar_radius *. cos (angle -. gap)
      ; y2 = radar_radius *. sin (angle -. gap)
      ; style = Style.make ~foreground:color ()
      }
  in
  List.map ring ring_radii
  @ [ spoke (radar_radius, center); spoke (center, radar_radius) ]
  @ [ Canvas.Points { coordinates = blips; style = blip_style } ]
  @ List.map beam (List.rev beam_trail)
;;

let radar model : Widget.t =
  fun area buffer ->
  let block = framed "Radar" in
  let inner = Block.inner block area in
  if Rect.is_empty inner
  then Block.widget block area buffer
  else (
    let x_bounds, y_bounds =
      square_bounds
        radar_marker
        ~width:inner.Rect.width
        ~height:inner.Rect.height
    in
    Canvas.widget
      (Canvas.make
         ~block
         ~marker:radar_marker
         ~labels:
           [ { Canvas.x = center; y = radar_radius; text = Line.of_string "N" }
           ]
         ~x_bounds
         ~y_bounds
         (radar_shapes model))
      area
      buffer)
;;

let charts model : Widget.t =
  Widget.split
    ~spacing:chart_gap
    ~direction:Layout.Horizontal
    [ Constraint.Fill chart_weight, history_chart model
    ; Constraint.Fill remaining_weight, radar model
    ]
;;

let log_block =
  framed
    ~padding:
      { Block.left = log_side_padding
      ; right = log_side_padding
      ; top = Int.zero
      ; bottom = Int.zero
      }
    "Logs"
;;

let log_paragraph model =
  Paragraph.make
    ~block:log_block
    ~wrap:Wrap.Word
    ~scroll:model.log_scroll
    log_text
;;

(* How wide and how tall the visible part of the log is on a body region. *)
let log_viewport body =
  let inner = Block.inner log_block body in
  inner.Rect.width, Int.max Int.one inner.Rect.height
;;

let logs model : Widget.t =
  fun area buffer ->
  let paragraph = log_paragraph model in
  let width, height = log_viewport area in
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
  | Overview | Charts -> "Click a tab | Tab switches tab | Esc quits"
  | Packages -> "Type to filter | Up, Down or click move | Esc quits"
  | Logs -> "Up, Down, PgUp, PgDn or the wheel scroll | Esc quits"
;;

let footer model =
  Paragraph.widget
    (Paragraph.make
       ~alignment:Alignment.Center
       [ Line.of_string ~style:hint_style (help_for model.tab) ])
;;

let quit_dialog =
  Paragraph.widget
    (Paragraph.make
       ~block:(framed "Quit?")
       ~alignment:Alignment.Center
       [ Line.of_string "Press y to quit, n to stay" ])
;;

let view model frame =
  let screen = Frame.area frame in
  let regions = regions screen in
  Frame.render_widget frame (Tabs.widget (tab_bar model)) ~area:regions.tab_bar;
  (match model.tab with
   | Overview -> Frame.render_widget frame (overview model) ~area:regions.body
   | Charts -> Frame.render_widget frame (charts model) ~area:regions.body
   | Logs -> Frame.render_widget frame (logs model) ~area:regions.body
   | Packages ->
     let filter_region, table_region = packages_regions regions.body in
     Frame.render_stateful_widget
       frame
       (Text_field.widget filter_field)
       ~area:filter_region
       model.filter;
     Frame.render_stateful_widget
       frame
       (Table.widget (packages_table model))
       ~area:table_region
       model.packages;
     if not model.confirming_quit
     then
       Frame.set_cursor
         frame
         (Text_field.cursor_position
            filter_field
            model.filter
            ~area:filter_region));
  Frame.render_widget frame (footer model) ~area:regions.footer;
  if model.confirming_quit
  then (
    let dialog_area =
      Rect.centered screen ~width:dialog_width ~height:dialog_height
    in
    Frame.render_widget frame Clear.widget ~area:dialog_area;
    Frame.render_widget frame quit_dialog ~area:dialog_area)
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

let scroll_logs model ~by ~screen =
  let width, height = log_viewport (regions screen).body in
  let line_count = Paragraph.line_count (log_paragraph model) ~width in
  let furthest = Int.max Int.zero (line_count - height) in
  { model with
    log_scroll = Int.min furthest (Int.max Int.zero (model.log_scroll + by))
  }
;;

let scroll_to_end model ~screen = scroll_logs model ~by:Int.max_int ~screen
let scroll_to_start model ~screen = scroll_logs model ~by:Int.min_int ~screen

let move_in_packages model ~by =
  let item_count = List.length (visible_packages model) in
  if by > Int.zero
  then Selection.select_next model.packages ~item_count
  else Selection.select_previous model.packages ~item_count;
  model
;;

let press_key model key ~screen =
  let page = snd (log_viewport (regions screen).body) in
  match model.tab, key with
  | Packages, Event.Up -> move_in_packages model ~by:(Int.neg Int.one)
  | Packages, Event.Down -> move_in_packages model ~by:Int.one
  | Logs, Event.Up -> scroll_logs model ~by:(Int.neg Int.one) ~screen
  | Logs, Event.Down -> scroll_logs model ~by:Int.one ~screen
  | Logs, Event.Page_up -> scroll_logs model ~by:(Int.neg page) ~screen
  | Logs, Event.Page_down -> scroll_logs model ~by:page ~screen
  | Logs, Event.Home -> scroll_to_start model ~screen
  | Logs, Event.End -> scroll_to_end model ~screen
  | (Overview | Charts | Packages | Logs), _ -> model
;;

let click model position ~screen =
  let regions = regions screen in
  match Tabs.index_at (tab_bar model) ~area:regions.tab_bar position with
  | Some index ->
    { model with
      tab = List.nth_opt tabs index |> Option.value ~default:model.tab
    }
  | None ->
    (match model.tab with
     | Packages ->
       let _, table_region = packages_regions regions.body in
       Table.index_at
         (packages_table model)
         model.packages
         ~area:table_region
         position
       |> Option.iter (fun row -> Selection.select model.packages (Some row));
       model
     | Overview | Charts | Logs -> model)
;;

let turn_wheel model action ~screen =
  let direction =
    match action with
    | Event.Scroll_up -> Int.neg Int.one
    | _ -> Int.one
  in
  match model.tab with
  | Packages -> move_in_packages model ~by:direction
  | Logs -> scroll_logs model ~by:(direction * wheel_rows) ~screen
  | Overview | Charts -> model
;;

(* Typing in the filter starts the table over from its first row. *)
let edit_filter model event =
  if Line_edit.handle model.filter event
  then (
    Selection.select model.packages (Some Int.zero);
    true)
  else false
;;

let answer_dialog model = function
  | Event.Key { key = Event.Char "y" | Event.Enter; _ } -> None
  | Event.Key { key = Event.Char "n" | Event.Escape; _ } ->
    Some { model with confirming_quit = false }
  | Event.Key _ | Event.Resize _ | Event.Paste _ | Event.Mouse _ -> Some model
;;

let update model event ~screen =
  if model.confirming_quit
  then answer_dialog model event
  else (
    match event with
    | Event.Key { key = Event.Escape; _ } ->
      Some { model with confirming_quit = true }
    | Event.Key { key = Event.Tab; _ } ->
      Some { model with tab = shifted_tab model.tab ~by:Int.one }
    | Event.Key { key = Event.Backtab; _ } ->
      Some { model with tab = shifted_tab model.tab ~by:(Int.neg Int.one) }
    | Event.Mouse { action = Event.Press Event.Left_button; position; _ } ->
      Some (click model position ~screen)
    | Event.Mouse
        { action = (Event.Scroll_up | Event.Scroll_down) as action; _ } ->
      Some (turn_wheel model action ~screen)
    | Event.Key { key; _ } ->
      if model.tab = Packages && edit_filter model event
      then Some model
      else (
        match model.tab, key with
        | (Overview | Charts | Logs), Event.Char "q" ->
          Some { model with confirming_quit = true }
        | (Overview | Charts | Logs), Event.Right ->
          Some { model with tab = shifted_tab model.tab ~by:Int.one }
        | (Overview | Charts | Logs), Event.Left ->
          Some { model with tab = shifted_tab model.tab ~by:(Int.neg Int.one) }
        | _ -> Some (press_key model key ~screen))
    | Event.Paste _ ->
      if model.tab = Packages then ignore (edit_filter model event);
      Some model
    | Event.Resize _ | Event.Mouse _ -> Some model)
;;

let rec run terminal events model =
  Terminal.draw terminal (view model);
  match Input.poll events ~timeout:tick_seconds with
  | None -> run terminal events (tick model)
  | Some event ->
    (match update model event ~screen:(Terminal.area terminal) with
     | None -> ()
     | Some updated -> run terminal events updated)
;;

let () =
  Unix_backend.with_terminal
    ~features:[ Unix_backend.Mouse; Unix_backend.Bracketed_paste ]
    (fun terminal -> run terminal (Input.create ()) (initial_model ()))
;;
