module Block = Camlembert.Block
module Buffer = Camlembert.Buffer
module Cell = Camlembert.Cell
module Color = Camlembert.Color
module Rect = Camlembert.Rect
module Sparkline = Camlembert.Sparkline
module Style = Camlembert.Style

let origin = 0
let ascending_width = 9
let one_row = 1
let two_rows = 2
let short_width = 5
let narrow_width = 3
let framed_height = 3
let tall_maximum = 4
let eighths_maximum = 8
let beyond_the_maximum = 20
let negative_value = -1
let max_series_length = 12
let max_value = 50

let levels =
  [ " "
  ; "\u{2581}"
  ; "\u{2582}"
  ; "\u{2583}"
  ; "\u{2584}"
  ; "\u{2585}"
  ; "\u{2586}"
  ; "\u{2587}"
  ; "\u{2588}"
  ]
;;

let ascending_values = List.init (succ eighths_maximum) Fun.id
let green = Style.make ~foreground:Color.Green ()
let area ~width ~height = Rect.make ~column:origin ~row:origin ~width ~height

let render sparkline ~width ~height =
  let region = area ~width ~height in
  let buffer = Buffer.create region in
  Sparkline.widget sparkline region buffer;
  buffer
;;

let lines_of sparkline ~width ~height =
  Buffer.to_lines (render sparkline ~width ~height)
;;

let test_each_value_is_a_bar_in_eighths_of_a_cell () =
  Alcotest.(check (list string))
    "rising bars"
    [ String.concat "" levels ]
    (lines_of
       (Sparkline.make ascending_values)
       ~width:ascending_width
       ~height:one_row)
;;

let test_bars_grow_taller_over_several_rows () =
  let values = List.init (succ tall_maximum) Fun.id in
  Alcotest.(check (list string))
    "two rows"
    [ "   \u{2584}\u{2588}"; " \u{2584}\u{2588}\u{2588}\u{2588}" ]
    (lines_of
       (Sparkline.make values)
       ~width:(succ tall_maximum)
       ~height:two_rows)
;;

let test_a_short_series_leaves_the_rest_blank () =
  Alcotest.(check (list string))
    "left aligned"
    [ "\u{2588}\u{2588}   " ]
    (lines_of
       (Sparkline.make [ eighths_maximum; eighths_maximum ])
       ~width:short_width
       ~height:one_row)
;;

let test_a_long_series_shows_its_last_values () =
  let values = [ 1; 2; 3; 4; 5 ] in
  Alcotest.(check (list string))
    "last three"
    [ "\u{2583}\u{2584}\u{2585}" ]
    (lines_of
       (Sparkline.make ~maximum:eighths_maximum values)
       ~width:narrow_width
       ~height:one_row)
;;

let test_the_maximum_can_be_given () =
  Alcotest.(check (list string))
    "half of the given maximum"
    [ "\u{2584}" ]
    (lines_of
       (Sparkline.make ~maximum:eighths_maximum [ tall_maximum ])
       ~width:one_row
       ~height:one_row);
  Alcotest.(check (list string))
    "values above it are cut"
    [ "\u{2588}" ]
    (lines_of
       (Sparkline.make ~maximum:eighths_maximum [ beyond_the_maximum ])
       ~width:one_row
       ~height:one_row)
;;

let test_a_series_of_zeros_draws_nothing () =
  Alcotest.(check (list string))
    "blank"
    [ "   " ]
    (lines_of (Sparkline.make [ 0; 0; 0 ]) ~width:narrow_width ~height:one_row)
;;

let test_the_style_applies_to_the_region () =
  let buffer =
    render
      (Sparkline.make ~style:green [ eighths_maximum ])
      ~width:narrow_width
      ~height:one_row
  in
  Alcotest.(check bool)
    "bar"
    true
    (Style.equal
       green
       (Buffer.get buffer ~column:origin ~row:origin).Cell.style)
;;

let test_a_block_frames_the_chart () =
  let block = Block.make ~borders:Block.all_sides () in
  Alcotest.(check (list string))
    "framed"
    [ "┌───┐"; "│\u{2588}\u{2588} │"; "└───┘" ]
    (lines_of
       (Sparkline.make ~block [ eighths_maximum; eighths_maximum ])
       ~width:short_width
       ~height:framed_height)
;;

let test_negative_values_are_rejected () =
  Alcotest.check_raises
    "negative value"
    (Invalid_argument "Sparkline.make: negative value")
    (fun () -> ignore (Sparkline.make [ 1; negative_value ]));
  Alcotest.check_raises
    "negative maximum"
    (Invalid_argument "Sparkline.make: negative value")
    (fun () -> ignore (Sparkline.make ~maximum:negative_value [ 1 ]))
;;

let level_of symbol = Option.get (List.find_index (String.equal symbol) levels)

let property_taller_values_never_give_shorter_bars =
  QCheck2.Test.make
    ~name:"larger values never give shorter bars"
    ~print:QCheck2.Print.(list int)
    QCheck2.Gen.(
      list_size
        (int_range Int.one max_series_length)
        (int_range Int.zero max_value))
    (fun values ->
       let ascending = List.sort compare values in
       let width = List.length ascending in
       let symbols =
         List.init width (fun column ->
           (Buffer.get
              (render (Sparkline.make ascending) ~width ~height:one_row)
              ~column
              ~row:origin)
             .Cell.symbol)
       in
       let rec nondecreasing = function
         | first :: (second :: _ as rest) ->
           level_of first <= level_of second && nondecreasing rest
         | _ -> true
       in
       nondecreasing symbols)
;;

let property_drawing_stays_inside_the_area =
  QCheck2.Test.make
    ~name:"a sparkline only changes cells inside its area"
    ~print:QCheck2.Print.(pair Generators.print_rect Generators.print_edits)
    QCheck2.Gen.(pair Generators.rect Generators.edits)
    (fun (region, edits) ->
       Support.leaves_the_outside_untouched
         (Sparkline.widget (Sparkline.make ascending_values))
         ~region
         ~edits)
;;

let () =
  Alcotest.run
    "Sparkline"
    [ ( "bars"
      , [ Alcotest.test_case
            "eighths"
            `Quick
            test_each_value_is_a_bar_in_eighths_of_a_cell
        ; Alcotest.test_case
            "several rows"
            `Quick
            test_bars_grow_taller_over_several_rows
        ; Alcotest.test_case
            "short series"
            `Quick
            test_a_short_series_leaves_the_rest_blank
        ; Alcotest.test_case
            "long series"
            `Quick
            test_a_long_series_shows_its_last_values
        ; Alcotest.test_case "maximum" `Quick test_the_maximum_can_be_given
        ; Alcotest.test_case "zeros" `Quick test_a_series_of_zeros_draws_nothing
        ] )
    ; ( "widget"
      , [ Alcotest.test_case "style" `Quick test_the_style_applies_to_the_region
        ; Alcotest.test_case "block" `Quick test_a_block_frames_the_chart
        ; Alcotest.test_case
            "negative values"
            `Quick
            test_negative_values_are_rejected
        ] )
    ; ( "properties"
      , List.map
          QCheck_alcotest.to_alcotest
          [ property_taller_values_never_give_shorter_bars
          ; property_drawing_stays_inside_the_area
          ] )
    ]
;;
