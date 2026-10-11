module Block = Camlembert.Block
module Buffer = Camlembert.Buffer
module Cell = Camlembert.Cell
module Color = Camlembert.Color
module Grapheme = Camlembert.Grapheme
module Line = Camlembert.Line
module Line_gauge = Camlembert.Line_gauge
module Rect = Camlembert.Rect
module Style = Camlembert.Style

let origin = 0
let line_width = 20
let one_row = 1
let three_rows = 3
let framed_width = 12
let label_only_width = 3
let narrow_width = 10
let label_width_of_half = 3
let label_gap = 1
let empty = 0.0
let quarter = 0.25
let half = 0.5
let full = 1.0
let above_the_range = 1.5
let below_the_range = -0.1
let label_text = "CPU"
let long_label_text = "abcdef"
let filled_symbol = "\u{2501}"
let unfilled_symbol = "\u{2500}"
let no_label = Line.of_string ""
let green = Style.make ~foreground:Color.Green ()
let red = Style.make ~foreground:Color.Red ()
let on_blue = Style.make ~background:Color.Blue ()
let bold = Style.make ~modifiers:[ Style.Bold ] ()
let style_testable = Alcotest.testable Style.pp Style.equal
let area ~width ~height = Rect.make ~column:origin ~row:origin ~width ~height

let render gauge ~width ~height =
  let region = area ~width ~height in
  let buffer = Buffer.create region in
  Line_gauge.widget gauge region buffer;
  buffer
;;

let lines_of gauge ~width ~height =
  Buffer.to_lines (render gauge ~width ~height)
;;

let blank_line = String.make line_width ' '
let first_line gauge ~width = List.hd (lines_of gauge ~width ~height:one_row)

(* How many cells of the first row show [symbol]. *)
let count_of symbol buffer ~width =
  List.init width Fun.id
  |> List.filter (fun column ->
    String.equal (Buffer.get buffer ~column ~row:origin).Cell.symbol symbol)
  |> List.length
;;

let test_the_default_label_is_the_percentage_before_the_line () =
  Alcotest.(check string)
    "empty"
    "0% ─────────────────"
    (first_line (Line_gauge.make ~ratio:empty ()) ~width:line_width);
  Alcotest.(check string)
    "half"
    "50% ━━━━━━━━────────"
    (first_line (Line_gauge.make ~ratio:half ()) ~width:line_width);
  Alcotest.(check string)
    "full"
    "100% ━━━━━━━━━━━━━━━"
    (first_line (Line_gauge.make ~ratio:full ()) ~width:line_width)
;;

let test_a_given_label_replaces_the_percentage () =
  Alcotest.(check string)
    "label"
    "CPU ━━━━────────────"
    (first_line
       (Line_gauge.make ~label:(Line.of_string label_text) ~ratio:quarter ())
       ~width:line_width)
;;

let test_without_a_label_the_line_takes_the_whole_row () =
  Alcotest.(check string)
    "no label"
    "━━━━━─────"
    (first_line
       (Line_gauge.make ~label:no_label ~ratio:half ())
       ~width:narrow_width)
;;

let test_only_the_first_row_is_drawn () =
  Alcotest.(check (list string))
    "rows"
    [ "50% ━━━━━━━━────────"; blank_line; blank_line ]
    (lines_of
       (Line_gauge.make ~ratio:half ())
       ~width:line_width
       ~height:three_rows)
;;

let test_a_label_wider_than_the_region_is_cut_and_the_line_is_left_out () =
  Alcotest.(check string)
    "cut"
    "abc"
    (first_line
       (Line_gauge.make ~label:(Line.of_string long_label_text) ~ratio:half ())
       ~width:label_only_width)
;;

let test_a_block_frames_the_gauge () =
  let block = Block.make ~borders:Block.all_sides () in
  Alcotest.(check (list string))
    "framed"
    [ "┌──────────┐"; "│50% ━━━───│"; "│          │"; "└──────────┘" ]
    (lines_of
       (Line_gauge.make ~block ~ratio:half ())
       ~width:framed_width
       ~height:(succ three_rows))
;;

let test_each_part_takes_its_own_style () =
  let buffer =
    render
      (Line_gauge.make
         ~style:on_blue
         ~filled_style:green
         ~unfilled_style:red
         ~label_style:bold
         ~ratio:half
         ())
      ~width:line_width
      ~height:one_row
  in
  let style_at column = (Buffer.get buffer ~column ~row:origin).Cell.style in
  let bar_start = label_width_of_half + label_gap in
  Alcotest.check
    style_testable
    "label"
    (Style.patch on_blue bold)
    (style_at origin);
  Alcotest.check
    style_testable
    "filled"
    (Style.patch on_blue green)
    (style_at bar_start);
  Alcotest.check
    style_testable
    "unfilled"
    (Style.patch on_blue red)
    (style_at (line_width - label_gap))
;;

let test_ratios_outside_of_zero_to_one_are_rejected () =
  List.iter
    (fun ratio ->
       Alcotest.check_raises
         "outside"
         (Invalid_argument "Line_gauge.make: ratio outside of 0 to 1")
         (fun () -> ignore (Line_gauge.make ~ratio ())))
    [ above_the_range; below_the_range; Float.nan ]
;;

let bar_width_without_label = narrow_width

let property_a_larger_ratio_never_fills_less =
  QCheck2.Test.make
    ~name:"a larger ratio never fills fewer cells"
    ~print:QCheck2.Print.(pair float float)
    QCheck2.Gen.(pair Generators.ratio Generators.ratio)
    (fun (first, second) ->
       let lower = Float.min first second in
       let higher = Float.max first second in
       let filled ratio =
         count_of
           filled_symbol
           (render
              (Line_gauge.make ~label:no_label ~ratio ())
              ~width:bar_width_without_label
              ~height:one_row)
           ~width:bar_width_without_label
       in
       filled lower <= filled higher && filled higher <= bar_width_without_label)
;;

let property_the_line_always_fills_the_whole_bar =
  QCheck2.Test.make
    ~name:"thick and thin cells together fill the whole bar"
    ~print:QCheck2.Print.float
    Generators.ratio
    (fun ratio ->
       let buffer =
         render
           (Line_gauge.make ~label:no_label ~ratio ())
           ~width:bar_width_without_label
           ~height:one_row
       in
       count_of filled_symbol buffer ~width:bar_width_without_label
       + count_of unfilled_symbol buffer ~width:bar_width_without_label
       = bar_width_without_label)
;;

let property_drawing_stays_inside_the_area =
  QCheck2.Test.make
    ~name:"a line gauge only changes cells inside its area"
    ~print:
      QCheck2.Print.(triple float Generators.print_rect Generators.print_edits)
    QCheck2.Gen.(triple Generators.ratio Generators.rect Generators.edits)
    (fun (ratio, region, edits) ->
       Support.leaves_the_outside_untouched
         (Line_gauge.widget (Line_gauge.make ~ratio ()))
         ~region
         ~edits)
;;

let () =
  Alcotest.run
    "Line gauge"
    [ ( "line"
      , [ Alcotest.test_case
            "percentage"
            `Quick
            test_the_default_label_is_the_percentage_before_the_line
        ; Alcotest.test_case
            "custom label"
            `Quick
            test_a_given_label_replaces_the_percentage
        ; Alcotest.test_case
            "no label"
            `Quick
            test_without_a_label_the_line_takes_the_whole_row
        ; Alcotest.test_case
            "first row only"
            `Quick
            test_only_the_first_row_is_drawn
        ; Alcotest.test_case
            "long label"
            `Quick
            test_a_label_wider_than_the_region_is_cut_and_the_line_is_left_out
        ; Alcotest.test_case "block" `Quick test_a_block_frames_the_gauge
        ; Alcotest.test_case "styles" `Quick test_each_part_takes_its_own_style
        ; Alcotest.test_case
            "invalid ratios"
            `Quick
            test_ratios_outside_of_zero_to_one_are_rejected
        ] )
    ; ( "properties"
      , List.map
          QCheck_alcotest.to_alcotest
          [ property_a_larger_ratio_never_fills_less
          ; property_the_line_always_fills_the_whole_bar
          ; property_drawing_stays_inside_the_area
          ] )
    ]
;;
