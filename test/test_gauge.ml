module Block = Camlembert.Block
module Buffer = Camlembert.Buffer
module Cell = Camlembert.Cell
module Color = Camlembert.Color
module Gauge = Camlembert.Gauge
module Line = Camlembert.Line
module Rect = Camlembert.Rect
module Style = Camlembert.Style

let origin = 0
let bar_width = 10
let bar_height = 1
let tall_bar_height = 3
let half = 0.5
let full = 1.0
let empty = 0.0
let slightly_over_half = 0.55
let above_the_range = 1.5
let below_the_range = -0.1
let filled_cells_at_half = 5
let middle_row_of_a_tall_bar = 1
let label_column = 3
let green_bar = Style.make ~foreground:Color.Green ()
let area ~width ~height = Rect.make ~column:origin ~row:origin ~width ~height

let render gauge ~width ~height =
  let region = area ~width ~height in
  let buffer = Buffer.create region in
  Gauge.widget gauge region buffer;
  buffer
;;

let lines_of gauge ~width ~height =
  Buffer.to_lines (render gauge ~width ~height)
;;

let is_reversed buffer ~column ~row =
  Style.Modifier_set.mem
    Style.Reversed
    (Buffer.get buffer ~column ~row).Cell.style.Style.modifiers
;;

(* How many cells of the first row are drawn with the colors swapped. *)
let filled_cells buffer ~width =
  List.init width Fun.id
  |> List.filter (fun column -> is_reversed buffer ~column ~row:origin)
  |> List.length
;;

let no_label = Line.of_string ""

let test_the_default_label_is_the_percentage () =
  Alcotest.(check (list string))
    "empty"
    [ "    0%    " ]
    (lines_of (Gauge.make ~ratio:empty ()) ~width:bar_width ~height:bar_height);
  Alcotest.(check (list string))
    "half"
    [ "   50%    " ]
    (lines_of (Gauge.make ~ratio:half ()) ~width:bar_width ~height:bar_height);
  Alcotest.(check (list string))
    "full"
    [ "   100%   " ]
    (lines_of (Gauge.make ~ratio:full ()) ~width:bar_width ~height:bar_height)
;;

let test_a_given_label_replaces_the_percentage () =
  Alcotest.(check (list string))
    "label"
    [ "   done   " ]
    (lines_of
       (Gauge.make ~label:(Line.of_string "done") ~ratio:half ())
       ~width:bar_width
       ~height:bar_height)
;;

let test_the_filled_part_follows_the_ratio () =
  let at ratio =
    filled_cells
      (render
         (Gauge.make ~label:no_label ~ratio ())
         ~width:bar_width
         ~height:bar_height)
      ~width:bar_width
  in
  Alcotest.(check int) "empty" Int.zero (at empty);
  Alcotest.(check int) "half" filled_cells_at_half (at half);
  Alcotest.(check int) "full" bar_width (at full)
;;

let test_the_label_is_drawn_over_the_filled_part () =
  let buffer =
    render (Gauge.make ~ratio:half ()) ~width:bar_width ~height:bar_height
  in
  Alcotest.(check bool)
    "label over the bar is reversed"
    true
    (is_reversed buffer ~column:label_column ~row:origin);
  Alcotest.(check bool)
    "label past the bar is not"
    false
    (is_reversed buffer ~column:(filled_cells_at_half + Int.one) ~row:origin)
;;

let test_the_last_cell_is_filled_partially () =
  let buffer =
    render
      (Gauge.make
         ~label:no_label
         ~gauge_style:green_bar
         ~ratio:slightly_over_half
         ())
      ~width:bar_width
      ~height:bar_height
  in
  let partial_cell =
    Buffer.get buffer ~column:filled_cells_at_half ~row:origin
  in
  Alcotest.(check string) "left half block" "\u{258C}" partial_cell.Cell.symbol;
  Alcotest.(check bool)
    "bar color"
    true
    (Style.equal green_bar partial_cell.Cell.style)
;;

let test_every_row_is_filled_and_the_label_sits_in_the_middle () =
  let buffer =
    render (Gauge.make ~ratio:full ()) ~width:bar_width ~height:tall_bar_height
  in
  List.iter
    (fun row ->
       Alcotest.(check bool)
         "filled row"
         true
         (is_reversed buffer ~column:origin ~row))
    (List.init tall_bar_height Fun.id);
  Alcotest.(check string)
    "label row"
    "   100%   "
    (List.nth (Buffer.to_lines buffer) middle_row_of_a_tall_bar)
;;

let test_a_block_frames_the_gauge () =
  let block = Block.make ~borders:Block.all_sides () in
  Alcotest.(check (list string))
    "framed"
    [ "┌────────┐"; "│  100%  │"; "└────────┘" ]
    (lines_of
       (Gauge.make ~block ~ratio:full ())
       ~width:bar_width
       ~height:tall_bar_height)
;;

let test_ratios_outside_of_zero_to_one_are_rejected () =
  List.iter
    (fun ratio ->
       Alcotest.check_raises
         "outside"
         (Invalid_argument "Gauge.make: ratio outside of 0 to 1")
         (fun () -> ignore (Gauge.make ~ratio ())))
    [ above_the_range; below_the_range; Float.nan ]
;;

let property_a_larger_ratio_never_fills_less =
  QCheck2.Test.make
    ~name:"a larger ratio never fills fewer cells"
    ~print:QCheck2.Print.(pair float float)
    QCheck2.Gen.(pair Generators.ratio Generators.ratio)
    (fun (first, second) ->
       let lower = Float.min first second in
       let higher = Float.max first second in
       let filled ratio =
         filled_cells
           (render
              (Gauge.make ~label:no_label ~ratio ())
              ~width:bar_width
              ~height:bar_height)
           ~width:bar_width
       in
       filled lower <= filled higher && filled higher <= bar_width)
;;

let property_drawing_stays_inside_the_area =
  QCheck2.Test.make
    ~name:"a gauge only changes cells inside its area"
    ~print:
      QCheck2.Print.(triple float Generators.print_rect Generators.print_edits)
    QCheck2.Gen.(triple Generators.ratio Generators.rect Generators.edits)
    (fun (ratio, region, edits) ->
       Support.leaves_the_outside_untouched
         (Gauge.widget (Gauge.make ~ratio ()))
         ~region
         ~edits)
;;

let () =
  Alcotest.run
    "Gauge"
    [ ( "label"
      , [ Alcotest.test_case
            "percentage"
            `Quick
            test_the_default_label_is_the_percentage
        ; Alcotest.test_case
            "custom label"
            `Quick
            test_a_given_label_replaces_the_percentage
        ] )
    ; ( "bar"
      , [ Alcotest.test_case
            "ratio"
            `Quick
            test_the_filled_part_follows_the_ratio
        ; Alcotest.test_case
            "label over the bar"
            `Quick
            test_the_label_is_drawn_over_the_filled_part
        ; Alcotest.test_case
            "partial cell"
            `Quick
            test_the_last_cell_is_filled_partially
        ; Alcotest.test_case
            "tall bar"
            `Quick
            test_every_row_is_filled_and_the_label_sits_in_the_middle
        ; Alcotest.test_case "block" `Quick test_a_block_frames_the_gauge
        ; Alcotest.test_case
            "invalid ratios"
            `Quick
            test_ratios_outside_of_zero_to_one_are_rejected
        ] )
    ; ( "properties"
      , List.map
          QCheck_alcotest.to_alcotest
          [ property_a_larger_ratio_never_fills_less
          ; property_drawing_stays_inside_the_area
          ] )
    ]
;;
