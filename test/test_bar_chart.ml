module Bar_chart = Camlembert.Bar_chart
module Block = Camlembert.Block
module Buffer = Camlembert.Buffer
module Cell = Camlembert.Cell
module Color = Camlembert.Color
module Rect = Camlembert.Rect
module Style = Camlembert.Style

let origin = 0
let chart_width = 11
let chart_height = 5
let two_bars_width = 7
let three_cells = 3
let short_chart_height = 2
let tall_value = 4
let short_value = 2
let no_value = 0
let beyond_the_maximum = 9
let eighths_maximum = 8
let three_eighths = 3
let negative_value = -1
let adjacent_bar_count = 2
let label_row = 1
let wide_bar_width = 5
let no_gap = 0
let framed_height = 4
let default_bar_width = 3
let default_bar_gap = 1
let max_bar_count = 5
let max_value = 30
let max_chart_height = 8
let marker = "#"
let green = Style.make ~foreground:Color.Green ()
let bar label value = { Bar_chart.label; value }
let three_bars = [ bar "A" tall_value; bar "B" short_value; bar "C" no_value ]
let area ~width ~height = Rect.make ~column:origin ~row:origin ~width ~height

let render chart ~width ~height =
  let region = area ~width ~height in
  let buffer = Buffer.create region in
  Bar_chart.widget chart region buffer;
  buffer
;;

let is_reversed cell =
  Style.Modifier_set.mem Style.Reversed cell.Cell.style.Style.modifiers
;;

(* The chart as characters: a marker where a bar fills a whole cell with
   nothing written on it, and the symbol that is drawn everywhere else. *)
let art chart ~width ~height =
  let buffer = render chart ~width ~height in
  List.init height (fun row ->
    List.init width (fun column ->
      let cell = Buffer.get buffer ~column ~row in
      if is_reversed cell && String.equal cell.Cell.symbol " "
      then marker
      else cell.Cell.symbol)
    |> String.concat "")
;;

let test_bars_grow_with_their_values_and_labels_sit_under_them () =
  Alcotest.(check (list string))
    "chart"
    [ "###        "
    ; "###        "
    ; "### ###    "
    ; "#4# #2#    "
    ; " A   B   C "
    ]
    (art (Bar_chart.make three_bars) ~width:chart_width ~height:chart_height)
;;

let test_a_bar_between_two_cells_uses_a_partial_block () =
  let chart =
    Bar_chart.make ~maximum:eighths_maximum [ bar "x" three_eighths ]
  in
  Alcotest.(check (list string))
    "three eighths on a one row bar"
    [ "\u{2583}\u{2583}\u{2583}"; " x " ]
    (art chart ~width:three_cells ~height:short_chart_height)
;;

let test_values_above_the_maximum_are_cut_to_the_full_height () =
  let chart =
    Bar_chart.make ~maximum:tall_value [ bar "x" beyond_the_maximum ]
  in
  Alcotest.(check (list string))
    "full"
    [ "###"; "#9#"; " x " ]
    (art chart ~width:three_cells ~height:three_cells)
;;

let test_a_bar_of_zero_draws_nothing () =
  let chart = Bar_chart.make [ bar "x" no_value ] in
  Alcotest.(check (list string))
    "label only"
    [ "   "; " x " ]
    (art chart ~width:three_cells ~height:short_chart_height)
;;

let test_bars_that_do_not_fit_are_left_out () =
  Alcotest.(check (list string))
    "two bars"
    [ "###    "; "#4# #2#"; " A   B " ]
    (art (Bar_chart.make three_bars) ~width:two_bars_width ~height:three_cells)
;;

let test_the_width_and_the_gap_can_be_changed () =
  let chart =
    Bar_chart.make
      ~bar_width:wide_bar_width
      ~bar_gap:no_gap
      [ bar "A" tall_value; bar "B" short_value ]
  in
  Alcotest.(check (list string))
    "wide bars without a gap"
    [ "#####     "; "##4####2##"; "  A    B  " ]
    (art chart ~width:(adjacent_bar_count * wide_bar_width) ~height:three_cells)
;;

let test_a_label_wider_than_its_bar_is_cut () =
  let chart = Bar_chart.make [ bar "abcdef" no_value ] in
  Alcotest.(check string)
    "cut"
    "abc"
    (List.nth
       (art chart ~width:three_cells ~height:short_chart_height)
       label_row)
;;

let test_the_style_of_the_bars_applies_to_full_cells () =
  let chart = Bar_chart.make ~bar_style:green [ bar "x" tall_value ] in
  let buffer = render chart ~width:three_cells ~height:short_chart_height in
  let style = (Buffer.get buffer ~column:origin ~row:origin).Cell.style in
  Alcotest.(check bool)
    "green and reversed"
    true
    (Style.equal
       (Style.patch green (Style.make ~modifiers:[ Style.Reversed ] ()))
       style)
;;

let test_a_block_frames_the_chart () =
  let block = Block.make ~borders:Block.all_sides () in
  let chart = Bar_chart.make ~block [ bar "x" tall_value ] in
  Alcotest.(check (list string))
    "framed"
    [ "┌───┐"; "│#4#│"; "│ x │"; "└───┘" ]
    (art chart ~width:(succ (succ three_cells)) ~height:framed_height)
;;

let test_invalid_charts_are_rejected () =
  let rejects message make =
    Alcotest.check_raises message (Invalid_argument message) (fun () ->
      ignore (make ()))
  in
  rejects "Bar_chart.make: negative value" (fun () ->
    Bar_chart.make [ bar "x" negative_value ]);
  rejects "Bar_chart.make: negative value" (fun () ->
    Bar_chart.make ~maximum:negative_value [ bar "x" short_value ]);
  rejects "Bar_chart.make: bar width below one" (fun () ->
    Bar_chart.make ~bar_width:no_value [ bar "x" short_value ]);
  rejects "Bar_chart.make: negative gap" (fun () ->
    Bar_chart.make ~bar_gap:negative_value [ bar "x" short_value ])
;;

(* How many cells of the first bar are filled, wholly or partly. *)
let filled_cells_of_bar chart ~index ~height =
  let buffer =
    render
      chart
      ~width:(max_bar_count * (default_bar_width + default_bar_gap))
      ~height
  in
  let column = index * (default_bar_width + default_bar_gap) in
  List.init (pred height) Fun.id
  |> List.filter (fun row ->
    let cell = Buffer.get buffer ~column ~row in
    is_reversed cell || not (String.equal cell.Cell.symbol " "))
  |> List.length
;;

let property_taller_values_never_give_shorter_bars =
  QCheck2.Test.make
    ~name:"a larger value never gives a shorter bar"
    ~print:QCheck2.Print.(pair (list int) int)
    QCheck2.Gen.(
      pair
        (list_size
           (int_range Int.one max_bar_count)
           (int_range Int.zero max_value))
        (int_range short_chart_height max_chart_height))
    (fun (values, height) ->
       let chart = Bar_chart.make (List.map (bar "x") values) in
       let measured =
         List.mapi
           (fun index value -> value, filled_cells_of_bar chart ~index ~height)
           values
       in
       List.for_all
         (fun (first_value, first_cells) ->
            List.for_all
              (fun (second_value, second_cells) ->
                 first_value > second_value || first_cells <= second_cells)
              measured)
         measured)
;;

let property_drawing_stays_inside_the_area =
  QCheck2.Test.make
    ~name:"a chart only changes cells inside its area"
    ~print:QCheck2.Print.(pair Generators.print_rect Generators.print_edits)
    QCheck2.Gen.(pair Generators.rect Generators.edits)
    (fun (region, edits) ->
       Support.leaves_the_outside_untouched
         (Bar_chart.widget (Bar_chart.make three_bars))
         ~region
         ~edits)
;;

let () =
  Alcotest.run
    "Bar_chart"
    [ ( "bars"
      , [ Alcotest.test_case
            "values and labels"
            `Quick
            test_bars_grow_with_their_values_and_labels_sit_under_them
        ; Alcotest.test_case
            "partial block"
            `Quick
            test_a_bar_between_two_cells_uses_a_partial_block
        ; Alcotest.test_case
            "above the maximum"
            `Quick
            test_values_above_the_maximum_are_cut_to_the_full_height
        ; Alcotest.test_case "zero" `Quick test_a_bar_of_zero_draws_nothing
        ; Alcotest.test_case
            "bars that do not fit"
            `Quick
            test_bars_that_do_not_fit_are_left_out
        ; Alcotest.test_case
            "width and gap"
            `Quick
            test_the_width_and_the_gap_can_be_changed
        ; Alcotest.test_case
            "long label"
            `Quick
            test_a_label_wider_than_its_bar_is_cut
        ] )
    ; ( "widget"
      , [ Alcotest.test_case
            "style"
            `Quick
            test_the_style_of_the_bars_applies_to_full_cells
        ; Alcotest.test_case "block" `Quick test_a_block_frames_the_chart
        ; Alcotest.test_case
            "invalid charts"
            `Quick
            test_invalid_charts_are_rejected
        ] )
    ; ( "properties"
      , List.map
          QCheck_alcotest.to_alcotest
          [ property_taller_values_never_give_shorter_bars
          ; property_drawing_stays_inside_the_area
          ] )
    ]
;;
