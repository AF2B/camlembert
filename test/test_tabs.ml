module Block = Camlembert.Block
module Buffer = Camlembert.Buffer
module Cell = Camlembert.Cell
module Color = Camlembert.Color
module Grapheme = Camlembert.Grapheme
module Line = Camlembert.Line
module Rect = Camlembert.Rect
module Style = Camlembert.Style
module Tabs = Camlembert.Tabs

let origin = 0
let room_width = 30
let narrow_room_width = 8
let one_row = 1
let framed_height = 3
let second_title_index = 1
let outside_of_the_titles = 10
let first_title_text_column = 1
let second_title_text_column = 7
let titles = List.map Line.of_string [ "One"; "Two"; "Three" ]
let highlight = Style.make ~foreground:Color.Yellow ()
let area ~width ~height = Rect.make ~column:origin ~row:origin ~width ~height

let render tabs ~width ~height =
  let region = area ~width ~height in
  let buffer = Buffer.create region in
  Tabs.widget tabs region buffer;
  buffer
;;

let first_line tabs ~width =
  List.hd (Buffer.to_lines (render tabs ~width ~height:one_row))
;;

let style_at buffer ~column = (Buffer.get buffer ~column ~row:origin).Cell.style
let pad text ~width = text ^ String.make (width - Grapheme.width text) ' '

let test_titles_have_spaces_around_them_and_dividers_between () =
  Alcotest.(check string)
    "row"
    (pad " One \u{2502} Two \u{2502} Three " ~width:room_width)
    (first_line (Tabs.make titles) ~width:room_width)
;;

let test_a_custom_divider () =
  Alcotest.(check string)
    "row"
    (pad " One / Two / Three " ~width:room_width)
    (first_line
       (Tabs.make ~divider:(Line.of_string "/") titles)
       ~width:room_width)
;;

let test_only_the_selected_title_is_highlighted () =
  let buffer =
    render
      (Tabs.make ~highlight_style:highlight ~selected:second_title_index titles)
      ~width:room_width
      ~height:one_row
  in
  Alcotest.(check bool)
    "selected title"
    true
    (Style.equal highlight (style_at buffer ~column:second_title_text_column));
  Alcotest.(check bool)
    "other title"
    true
    (Style.equal
       Style.default
       (style_at buffer ~column:first_title_text_column));
  Alcotest.(check bool)
    "padding of the selected title"
    true
    (Style.equal
       Style.default
       (style_at buffer ~column:(pred second_title_text_column)))
;;

let test_the_first_title_is_selected_by_default () =
  let buffer =
    render
      (Tabs.make ~highlight_style:highlight titles)
      ~width:room_width
      ~height:one_row
  in
  Alcotest.(check bool)
    "first title"
    true
    (Style.equal highlight (style_at buffer ~column:first_title_text_column))
;;

let test_nothing_is_highlighted_when_the_selection_is_out_of_range () =
  let buffer =
    render
      (Tabs.make
         ~highlight_style:highlight
         ~selected:outside_of_the_titles
         titles)
      ~width:room_width
      ~height:one_row
  in
  Alcotest.(check bool)
    "no highlight"
    true
    (List.for_all
       (fun column -> Style.equal Style.default (style_at buffer ~column))
       (List.init room_width Fun.id))
;;

let test_what_does_not_fit_is_cut () =
  Alcotest.(check string)
    "cut"
    " One \u{2502} T"
    (first_line (Tabs.make titles) ~width:narrow_room_width)
;;

let test_a_block_frames_the_tabs () =
  let block = Block.make ~borders:Block.all_sides () in
  let lines =
    Buffer.to_lines
      (render (Tabs.make ~block titles) ~width:room_width ~height:framed_height)
  in
  Alcotest.(check string)
    "titles inside the frame"
    ("\u{2502}"
     ^ pad " One \u{2502} Two \u{2502} Three " ~width:(room_width - 2)
     ^ "\u{2502}")
    (List.nth lines one_row)
;;

let property_drawing_stays_inside_the_area =
  QCheck2.Test.make
    ~name:"tabs only change cells inside their area"
    ~print:QCheck2.Print.(pair Generators.print_rect Generators.print_edits)
    QCheck2.Gen.(pair Generators.rect Generators.edits)
    (fun (region, edits) ->
       Support.leaves_the_outside_untouched
         (Tabs.widget (Tabs.make titles))
         ~region
         ~edits)
;;

let () =
  Alcotest.run
    "Tabs"
    [ ( "titles"
      , [ Alcotest.test_case
            "spaces and dividers"
            `Quick
            test_titles_have_spaces_around_them_and_dividers_between
        ; Alcotest.test_case "custom divider" `Quick test_a_custom_divider
        ; Alcotest.test_case "cut" `Quick test_what_does_not_fit_is_cut
        ; Alcotest.test_case "block" `Quick test_a_block_frames_the_tabs
        ] )
    ; ( "selection"
      , [ Alcotest.test_case
            "highlight"
            `Quick
            test_only_the_selected_title_is_highlighted
        ; Alcotest.test_case
            "first by default"
            `Quick
            test_the_first_title_is_selected_by_default
        ; Alcotest.test_case
            "out of range"
            `Quick
            test_nothing_is_highlighted_when_the_selection_is_out_of_range
        ] )
    ; ( "properties"
      , [ QCheck_alcotest.to_alcotest property_drawing_stays_inside_the_area ] )
    ]
;;
