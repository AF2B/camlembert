module Block = Camlembert.Block
module Buffer = Camlembert.Buffer
module Cell = Camlembert.Cell
module Chart = Camlembert.Chart
module Color = Camlembert.Color
module Grapheme = Camlembert.Grapheme
module Marker = Camlembert.Marker
module Rect = Camlembert.Rect
module Style = Camlembert.Style

let origin = 0
let one_cell = 1
let lowest_x = 0.0
let middle_x = 5.0
let highest_x = 10.0
let lowest_y = 0.0
let middle_y = 50.0
let highest_y = 100.0
let x_bounds = lowest_x, highest_x
let y_bounds = lowest_y, highest_y
let x_labels = [ "0"; "5"; "10" ]
let y_labels = [ "0"; "50"; "100" ]
let x_title = "t"
let y_title = "v"
let unnamed = ""
let labeled_width = 12
let labeled_height = 7
let titled_height = 9
let framed_width = 14
let framed_height = 9
let plain_width = 11
let plain_height = 7
let too_narrow_width = 4
let too_short_height = 2
let y_axis_column = 3
let plot_column = 4
let top_row = 0
let bottom_plot_row = 4
let x_axis_row = 5
let x_labels_row = 6
let legend_column = 8
let legend_first_row = 0
let legend_second_row = 1
let legend_blank_column = 10
let full = "\u{2588}"
let vertical_line = "\u{2502}"
let horizontal_line = "\u{2500}"
let corner = "\u{2514}"
let red = Style.make ~foreground:Color.Red ()
let blue = Style.make ~foreground:Color.Blue ()
let on_green = Style.make ~background:Color.Green ()
let style_testable = Alcotest.testable Style.pp Style.equal
let area ~width ~height = Rect.make ~column:origin ~row:origin ~width ~height

let render chart ~width ~height =
  let region = area ~width ~height in
  let buffer = Buffer.create region in
  Chart.widget chart region buffer;
  buffer
;;

let lines_of chart ~width ~height =
  Buffer.to_lines (render chart ~width ~height)
;;

let style_at buffer ~column ~row = (Buffer.get buffer ~column ~row).Cell.style
let labeled_x_axis = Chart.axis ~labels:x_labels ~bounds:x_bounds ()
let labeled_y_axis = Chart.axis ~labels:y_labels ~bounds:y_bounds ()
let plain_x_axis = Chart.axis ~bounds:x_bounds ()
let plain_y_axis = Chart.axis ~bounds:y_bounds ()

let labeled ?style datasets =
  Chart.make ?style ~x_axis:labeled_x_axis ~y_axis:labeled_y_axis datasets
;;

let plain ?legend datasets =
  Chart.make ?legend ~x_axis:plain_x_axis ~y_axis:plain_y_axis datasets
;;

let labeled_lines datasets =
  lines_of (labeled datasets) ~width:labeled_width ~height:labeled_height
;;

let plain_lines ?legend datasets =
  lines_of (plain ?legend datasets) ~width:plain_width ~height:plain_height
;;

(* A dataset that draws nothing, only to have a name in the legend. *)
let named name = Chart.dataset ~name []

(* A dataset where every dot is a cell, so pictures are easy to read. *)
let blocks ?graph_type ?style points =
  Chart.dataset ~marker:Marker.Block ?graph_type ?style ~name:unnamed points
;;

let empty_plain_picture =
  [ "│          "
  ; "│          "
  ; "│          "
  ; "│          "
  ; "│          "
  ; "│          "
  ; "└──────────"
  ]
;;

let test_axes_have_lines_a_corner_and_spread_labels () =
  Alcotest.(check (list string))
    "axes"
    [ "100│        "
    ; "   │        "
    ; " 50│        "
    ; "   │        "
    ; "  0│        "
    ; "   └────────"
    ; "    0   5 10"
    ]
    (labeled_lines [])
;;

let test_titles_go_above_the_y_axis_and_at_the_end_of_the_x_axis () =
  let chart =
    Chart.make
      ~x_axis:(Chart.axis ~title:x_title ~labels:x_labels ~bounds:x_bounds ())
      ~y_axis:(Chart.axis ~title:y_title ~labels:y_labels ~bounds:y_bounds ())
      []
  in
  Alcotest.(check (list string))
    "titles"
    [ "v           "
    ; "100│        "
    ; "   │        "
    ; " 50│        "
    ; "   │        "
    ; "  0│        "
    ; "   └────────"
    ; "    0   5 10"
    ; "           t"
    ]
    (lines_of chart ~width:labeled_width ~height:titled_height)
;;

let test_a_block_frames_the_chart () =
  let chart =
    Chart.make
      ~block:(Block.make ~borders:Block.all_sides ())
      ~x_axis:labeled_x_axis
      ~y_axis:labeled_y_axis
      []
  in
  Alcotest.(check (list string))
    "framed"
    [ "┌────────────┐"
    ; "│100│        │"
    ; "│   │        │"
    ; "│ 50│        │"
    ; "│   │        │"
    ; "│  0│        │"
    ; "│   └────────│"
    ; "│    0   5 10│"
    ; "└────────────┘"
    ]
    (lines_of chart ~width:framed_width ~height:framed_height)
;;

let test_without_room_for_the_plot_nothing_is_drawn () =
  let blank ~width ~height =
    List.init height (Fun.const (String.make width ' '))
  in
  Alcotest.(check (list string))
    "too narrow"
    (blank ~width:too_narrow_width ~height:labeled_height)
    (lines_of (labeled []) ~width:too_narrow_width ~height:labeled_height);
  Alcotest.(check (list string))
    "too short"
    (blank ~width:labeled_width ~height:too_short_height)
    (lines_of (labeled []) ~width:labeled_width ~height:too_short_height)
;;

let test_points_land_on_the_rows_of_their_labels () =
  Alcotest.(check (list string))
    "points"
    [ "100│       █"
    ; "   │        "
    ; " 50│    █   "
    ; "   │        "
    ; "  0│█       "
    ; "   └────────"
    ; "    0   5 10"
    ]
    (labeled_lines
       [ blocks [ lowest_x, lowest_y; middle_x, middle_y; highest_x, highest_y ]
       ])
;;

let test_connected_points_are_joined_by_a_line () =
  Alcotest.(check (list string))
    "line"
    [ "100│       █"
    ; "   │     ██ "
    ; " 50│   ██   "
    ; "   │ ██     "
    ; "  0│█       "
    ; "   └────────"
    ; "    0   5 10"
    ]
    (labeled_lines
       [ blocks
           ~graph_type:Chart.Connected
           [ lowest_x, lowest_y; highest_x, highest_y ]
       ])
;;

let test_datasets_are_drawn_with_their_markers () =
  let flat_line =
    Chart.dataset
      ~graph_type:Chart.Connected
      ~name:unnamed
      [ lowest_x, lowest_y; highest_x, lowest_y ]
  in
  Alcotest.(check (list string))
    "braille"
    [ "100│        "
    ; "   │        "
    ; " 50│        "
    ; "   │        "
    ; "  0│⣀⣀⣀⣀⣀⣀⣀⣀"
    ; "   └────────"
    ; "    0   5 10"
    ]
    (labeled_lines [ flat_line ])
;;

let test_a_later_dataset_is_drawn_over_an_earlier_one () =
  let at_lowest style = blocks ~style [ lowest_x, lowest_y ] in
  let buffer =
    render
      (labeled [ at_lowest red; at_lowest blue ])
      ~width:labeled_width
      ~height:labeled_height
  in
  Alcotest.check
    style_testable
    "the later one wins"
    blue
    (style_at buffer ~column:plot_column ~row:bottom_plot_row)
;;

let test_the_legend_sits_in_the_chosen_corner () =
  let names = [ named "up"; named "mid" ] in
  Alcotest.(check (list string))
    "top left"
    [ "│up        "
    ; "│mid       "
    ; "│          "
    ; "│          "
    ; "│          "
    ; "│          "
    ; "└──────────"
    ]
    (plain_lines ~legend:Chart.Top_left names);
  Alcotest.(check (list string))
    "top right"
    [ "│       up "
    ; "│       mid"
    ; "│          "
    ; "│          "
    ; "│          "
    ; "│          "
    ; "└──────────"
    ]
    (plain_lines ~legend:Chart.Top_right names);
  Alcotest.(check (list string))
    "bottom left"
    [ "│          "
    ; "│          "
    ; "│          "
    ; "│          "
    ; "│up        "
    ; "│mid       "
    ; "└──────────"
    ]
    (plain_lines ~legend:Chart.Bottom_left names);
  Alcotest.(check (list string))
    "bottom right"
    [ "│          "
    ; "│          "
    ; "│          "
    ; "│          "
    ; "│       up "
    ; "│       mid"
    ; "└──────────"
    ]
    (plain_lines ~legend:Chart.Bottom_right names)
;;

let test_the_legend_is_in_the_top_right_corner_by_default () =
  let names = [ named "up"; named "mid" ] in
  Alcotest.(check (list string))
    "default"
    (plain_lines ~legend:Chart.Top_right names)
    (plain_lines names)
;;

let test_the_legend_can_be_hidden () =
  Alcotest.(check (list string))
    "hidden"
    empty_plain_picture
    (plain_lines ~legend:Chart.Hidden [ named "up"; named "mid" ])
;;

let test_a_legend_wider_than_a_third_of_the_plot_is_dropped () =
  Alcotest.(check (list string))
    "too wide"
    empty_plain_picture
    (plain_lines [ named "four" ])
;;

let test_a_legend_taller_than_a_third_of_the_plot_is_dropped () =
  Alcotest.(check (list string))
    "too tall"
    empty_plain_picture
    (plain_lines [ named "a"; named "b"; named "c" ])
;;

let test_unnamed_datasets_stay_out_of_the_legend () =
  Alcotest.(check (list string))
    "only the named one"
    [ "│       mid"
    ; "│          "
    ; "│          "
    ; "│          "
    ; "│          "
    ; "│          "
    ; "└──────────"
    ]
    (plain_lines [ named unnamed; named "mid" ])
;;

let test_legend_entries_take_the_style_of_their_datasets () =
  let buffer =
    render
      (plain
         [ Chart.dataset ~style:red ~name:"up" []
         ; Chart.dataset ~style:blue ~name:"mid" []
         ])
      ~width:plain_width
      ~height:plain_height
  in
  Alcotest.check
    style_testable
    "first entry"
    red
    (style_at buffer ~column:legend_column ~row:legend_first_row);
  Alcotest.check
    style_testable
    "second entry"
    blue
    (style_at buffer ~column:legend_column ~row:legend_second_row)
;;

let test_the_chart_style_fills_the_region_and_the_legend () =
  let chart =
    Chart.make
      ~style:on_green
      ~x_axis:plain_x_axis
      ~y_axis:plain_y_axis
      [ Chart.dataset ~style:red ~name:"up" []
      ; Chart.dataset ~style:blue ~name:"mid" []
      ]
  in
  let buffer = render chart ~width:plain_width ~height:plain_height in
  Alcotest.check
    style_testable
    "a name over the background"
    (Style.patch on_green red)
    (style_at buffer ~column:legend_column ~row:legend_first_row);
  Alcotest.check
    style_testable
    "a blank cell of the legend"
    on_green
    (style_at buffer ~column:legend_blank_column ~row:legend_first_row);
  Alcotest.check
    style_testable
    "an empty cell of the plot"
    on_green
    (style_at buffer ~column:plot_column ~row:bottom_plot_row)
;;

let test_axes_take_their_own_styles () =
  let chart =
    Chart.make
      ~x_axis:(Chart.axis ~style:blue ~labels:x_labels ~bounds:x_bounds ())
      ~y_axis:(Chart.axis ~style:red ~labels:y_labels ~bounds:y_bounds ())
      []
  in
  let buffer = render chart ~width:labeled_width ~height:labeled_height in
  Alcotest.check
    style_testable
    "y axis line"
    red
    (style_at buffer ~column:y_axis_column ~row:top_row);
  Alcotest.check
    style_testable
    "y axis label"
    red
    (style_at buffer ~column:origin ~row:top_row);
  Alcotest.check
    style_testable
    "x axis line"
    blue
    (style_at buffer ~column:plot_column ~row:x_axis_row);
  Alcotest.check
    style_testable
    "x axis label"
    blue
    (style_at buffer ~column:plot_column ~row:x_labels_row)
;;

let test_axis_bounds_must_be_finite_and_ascending () =
  let rejects description bounds =
    Alcotest.check_raises
      description
      (Invalid_argument
         "Chart.axis: bounds must be finite, the lowest one first")
      (fun () -> ignore (Chart.axis ~bounds ()))
  in
  rejects "descending" (highest_x, lowest_x);
  rejects "empty" (lowest_x, lowest_x);
  rejects "not a number" (Float.nan, highest_x);
  rejects "infinite" (lowest_x, Float.infinity)
;;

let max_label_count = 4
let max_point_count = 5
let max_dataset_count = 3
let wide_range = 300.0
let smallest_span = 0.5
let smallest_side = 2
let largest_side = 30
let graph_types = [ Chart.Scatter; Chart.Connected ]

let legends =
  [ Chart.Top_left
  ; Chart.Top_right
  ; Chart.Bottom_left
  ; Chart.Bottom_right
  ; Chart.Hidden
  ]
;;

let print_graph_type = function
  | Chart.Scatter -> "Scatter"
  | Chart.Connected -> "Connected"
;;

let print_legend = function
  | Chart.Top_left -> "Top_left"
  | Chart.Top_right -> "Top_right"
  | Chart.Bottom_left -> "Bottom_left"
  | Chart.Bottom_right -> "Bottom_right"
  | Chart.Hidden -> "Hidden"
;;

let wide_float = QCheck2.Gen.float_range (Float.neg wide_range) wide_range

(* Axes and datasets are generated as plain data, so a failing case can be
   printed. *)
let axis_spec =
  QCheck2.Gen.(
    triple
      (option Generators.printable_ascii_text)
      (list_size
         (int_range Int.zero max_label_count)
         Generators.printable_ascii_text)
      (pair wide_float (float_range smallest_span wide_range)))
;;

let print_axis_spec =
  QCheck2.Print.(triple (option string) (list string) (pair float float))
;;

let axis_of (title, labels, (low, span)) =
  Chart.axis ?title ~labels ~bounds:(low, low +. span) ()
;;

let dataset_spec =
  QCheck2.Gen.(
    quad
      Generators.marker
      (oneof_list graph_types)
      Generators.printable_ascii_text
      (list_size
         (int_range Int.zero max_point_count)
         (pair wide_float wide_float)))
;;

let print_dataset_spec =
  QCheck2.Print.(
    quad
      Generators.print_marker
      print_graph_type
      string
      (list (pair float float)))
;;

let dataset_of (marker, graph_type, name, points) =
  Chart.dataset ~marker ~graph_type ~name points
;;

let datasets_spec =
  QCheck2.Gen.(list_size (int_range Int.zero max_dataset_count) dataset_spec)
;;

let side = QCheck2.Gen.int_range smallest_side largest_side

let property_a_chart_stays_inside_its_area =
  QCheck2.Test.make
    ~name:"a chart draws only inside its area"
    ~print:
      QCheck2.Print.(
        pair
          (quad
             print_axis_spec
             print_axis_spec
             print_legend
             (list print_dataset_spec))
          (pair Generators.print_rect Generators.print_edits))
    QCheck2.Gen.(
      pair
        (quad axis_spec axis_spec (oneof_list legends) datasets_spec)
        (pair Generators.rect Generators.edits))
    (fun ((x_axis, y_axis, legend, datasets), (region, edits)) ->
       let chart =
         Chart.make
           ~legend
           ~x_axis:(axis_of x_axis)
           ~y_axis:(axis_of y_axis)
           (List.map dataset_of datasets)
       in
       Support.leaves_the_outside_untouched (Chart.widget chart) ~region ~edits)
;;

let count_of symbol buffer =
  Buffer.to_lines buffer
  |> List.concat_map Grapheme.clusters
  |> List.filter (String.equal symbol)
  |> List.length
;;

let property_a_point_inside_the_bounds_is_drawn_once =
  QCheck2.Test.make
    ~name:"a point inside the bounds is drawn once"
    ~print:QCheck2.Print.(quad float float int int)
    QCheck2.Gen.(
      quad
        (float_range lowest_x highest_x)
        (float_range lowest_y highest_y)
        side
        side)
    (fun (x, y, width, height) ->
       let chart = plain ~legend:Chart.Hidden [ blocks [ x, y ] ] in
       count_of full (render chart ~width ~height) = one_cell)
;;

let property_the_axes_are_never_covered =
  QCheck2.Test.make
    ~name:"datasets and the legend never cover the axes"
    ~print:QCheck2.Print.(quad (list print_dataset_spec) print_legend int int)
    QCheck2.Gen.(quad datasets_spec (oneof_list legends) side side)
    (fun (datasets, legend, width, height) ->
       let chart = plain ~legend (List.map dataset_of datasets) in
       let buffer = render chart ~width ~height in
       let symbol_at ~column ~row =
         (Buffer.get buffer ~column ~row).Cell.symbol
       in
       let bottom_row = pred height in
       List.for_all
         (fun row -> String.equal (symbol_at ~column:origin ~row) vertical_line)
         (List.init bottom_row Fun.id)
       && String.equal (symbol_at ~column:origin ~row:bottom_row) corner
       && List.for_all
            (fun offset ->
               String.equal
                 (symbol_at ~column:(succ offset) ~row:bottom_row)
                 horizontal_line)
            (List.init (pred width) Fun.id))
;;

let () =
  Alcotest.run
    "Chart"
    [ ( "layout"
      , [ Alcotest.test_case
            "axes"
            `Quick
            test_axes_have_lines_a_corner_and_spread_labels
        ; Alcotest.test_case
            "titles"
            `Quick
            test_titles_go_above_the_y_axis_and_at_the_end_of_the_x_axis
        ; Alcotest.test_case "block" `Quick test_a_block_frames_the_chart
        ; Alcotest.test_case
            "no room"
            `Quick
            test_without_room_for_the_plot_nothing_is_drawn
        ; Alcotest.test_case
            "invalid bounds"
            `Quick
            test_axis_bounds_must_be_finite_and_ascending
        ] )
    ; ( "datasets"
      , [ Alcotest.test_case
            "points"
            `Quick
            test_points_land_on_the_rows_of_their_labels
        ; Alcotest.test_case
            "line"
            `Quick
            test_connected_points_are_joined_by_a_line
        ; Alcotest.test_case
            "markers"
            `Quick
            test_datasets_are_drawn_with_their_markers
        ; Alcotest.test_case
            "later on top"
            `Quick
            test_a_later_dataset_is_drawn_over_an_earlier_one
        ] )
    ; ( "legend"
      , [ Alcotest.test_case
            "corners"
            `Quick
            test_the_legend_sits_in_the_chosen_corner
        ; Alcotest.test_case
            "default corner"
            `Quick
            test_the_legend_is_in_the_top_right_corner_by_default
        ; Alcotest.test_case "hidden" `Quick test_the_legend_can_be_hidden
        ; Alcotest.test_case
            "too wide"
            `Quick
            test_a_legend_wider_than_a_third_of_the_plot_is_dropped
        ; Alcotest.test_case
            "too tall"
            `Quick
            test_a_legend_taller_than_a_third_of_the_plot_is_dropped
        ; Alcotest.test_case
            "unnamed"
            `Quick
            test_unnamed_datasets_stay_out_of_the_legend
        ] )
    ; ( "styles"
      , [ Alcotest.test_case
            "legend entries"
            `Quick
            test_legend_entries_take_the_style_of_their_datasets
        ; Alcotest.test_case
            "chart style"
            `Quick
            test_the_chart_style_fills_the_region_and_the_legend
        ; Alcotest.test_case "axes" `Quick test_axes_take_their_own_styles
        ] )
    ; ( "properties"
      , List.map
          QCheck_alcotest.to_alcotest
          [ property_a_chart_stays_inside_its_area
          ; property_a_point_inside_the_bounds_is_drawn_once
          ; property_the_axes_are_never_covered
          ] )
    ]
;;
