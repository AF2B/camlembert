module Alignment = Camlembert.Alignment
module Block = Camlembert.Block
module Buffer = Camlembert.Buffer
module Cell = Camlembert.Cell
module Color = Camlembert.Color
module Line = Camlembert.Line
module Paragraph = Camlembert.Paragraph
module Rect = Camlembert.Rect
module Style = Camlembert.Style
module Text = Camlembert.Text
module Wrap = Camlembert.Wrap

let origin = 0
let room_width = 6
let room_height = 3
let short_room_height = 2
let narrow_room_width = 5
let blank_character = ' '
let short_word = "ab"
let long_word = "abcdefgh"
let wide_word = "\u{6F22}"
let area ~width ~height = Rect.make ~column:origin ~row:origin ~width ~height

let render paragraph ~width ~height =
  let region = area ~width ~height in
  let buffer = Buffer.create region in
  Paragraph.widget paragraph region buffer;
  buffer
;;

let lines_of paragraph ~width ~height =
  Buffer.to_lines (render paragraph ~width ~height)
;;

let blank_line ~width = String.make width blank_character
let text_of lines = Text.of_string (String.concat "\n" lines)

let test_lines_go_on_separate_rows () =
  let paragraph = Paragraph.make (text_of [ "one"; "two" ]) in
  Alcotest.(check (list string))
    "rows"
    [ "one   "; "two   "; blank_line ~width:room_width ]
    (lines_of paragraph ~width:room_width ~height:room_height)
;;

let test_paragraph_alignment () =
  let lines_with alignment =
    lines_of
      (Paragraph.make ~alignment (text_of [ short_word ]))
      ~width:room_width
      ~height:Int.one
  in
  Alcotest.(check (list string)) "left" [ "ab    " ] (lines_with Alignment.Left);
  Alcotest.(check (list string))
    "center"
    [ "  ab  " ]
    (lines_with Alignment.Center);
  Alcotest.(check (list string))
    "right"
    [ "    ab" ]
    (lines_with Alignment.Right)
;;

let test_line_alignment_overrides_the_paragraph () =
  let line = Line.of_string ~alignment:Alignment.Right short_word in
  let paragraph = Paragraph.make ~alignment:Alignment.Left [ line ] in
  Alcotest.(check (list string))
    "right"
    [ "    ab" ]
    (lines_of paragraph ~width:room_width ~height:Int.one)
;;

let test_wide_characters_are_centered_by_columns () =
  let paragraph =
    Paragraph.make ~alignment:Alignment.Center (text_of [ wide_word ])
  in
  Alcotest.(check (list string))
    "centered"
    [ "  " ^ wide_word ^ "  " ]
    (lines_of paragraph ~width:room_width ~height:Int.one)
;;

let test_long_lines_are_cut () =
  let paragraph = Paragraph.make (text_of [ long_word ]) in
  Alcotest.(check (list string))
    "cut"
    [ "abcde" ]
    (lines_of paragraph ~width:narrow_room_width ~height:Int.one)
;;

let test_rows_beyond_the_bottom_are_left_out () =
  let paragraph = Paragraph.make (text_of [ "one"; "two"; "three" ]) in
  Alcotest.(check (list string))
    "first rows only"
    [ "one   "; "two   " ]
    (lines_of paragraph ~width:room_width ~height:short_room_height)
;;

let test_text_stays_inside_the_block () =
  let block = Block.make ~borders:Block.all_sides () in
  let paragraph = Paragraph.make ~block (text_of [ "Hi"; long_word ]) in
  Alcotest.(check (list string))
    "framed"
    [ "┌────┐"; "│Hi  │"; "│abcd│"; "└────┘" ]
    (lines_of paragraph ~width:room_width ~height:(succ room_height))
;;

let test_style_covers_the_text_area_only () =
  let background = Style.make ~background:Color.Blue () in
  let block = Block.make ~borders:Block.all_sides () in
  let paragraph =
    Paragraph.make ~block ~style:background (text_of [ short_word ])
  in
  let buffer = render paragraph ~width:room_width ~height:room_height in
  let style_at ~column ~row = (Buffer.get buffer ~column ~row).Cell.style in
  Alcotest.(check bool)
    "inside"
    true
    (Style.equal background (style_at ~column:Int.one ~row:Int.one));
  Alcotest.(check bool)
    "border"
    true
    (Style.equal Style.default (style_at ~column:origin ~row:origin))
;;

let test_empty_area_draws_nothing () =
  let empty = area ~width:Int.zero ~height:Int.zero in
  let buffer = Buffer.create (area ~width:room_width ~height:room_height) in
  Paragraph.widget (Paragraph.make (text_of [ short_word ])) empty buffer;
  Alcotest.(check (list string))
    "blank"
    (List.init room_height (fun _ -> blank_line ~width:room_width))
    (Buffer.to_lines buffer)
;;

let wrapping_room_width = 10
let wrapping_room_height = 3
let scrolled_rows = 1
let scrolled_past_the_end = 10
let negative_scroll = -1
let quick_fox = "the quick brown fox"

let test_wrapping_breaks_long_lines_into_rows () =
  let paragraph = Paragraph.make ~wrap:Wrap.Word (text_of [ quick_fox ]) in
  Alcotest.(check (list string))
    "wrapped"
    [ "the quick "; "brown fox "; blank_line ~width:wrapping_room_width ]
    (lines_of paragraph ~width:wrapping_room_width ~height:wrapping_room_height)
;;

let test_scrolling_hides_the_first_rows () =
  let paragraph =
    Paragraph.make ~scroll:scrolled_rows (text_of [ "one"; "two"; "three" ])
  in
  Alcotest.(check (list string))
    "from the second row"
    [ "two   "; "three " ]
    (lines_of paragraph ~width:room_width ~height:short_room_height)
;;

let test_scrolling_counts_rows_after_wrapping () =
  let paragraph =
    Paragraph.make ~wrap:Wrap.Word ~scroll:scrolled_rows (text_of [ quick_fox ])
  in
  Alcotest.(check (list string))
    "second wrapped row first"
    [ "brown fox "; blank_line ~width:wrapping_room_width ]
    (lines_of paragraph ~width:wrapping_room_width ~height:short_room_height)
;;

let test_scrolling_past_the_end_shows_nothing () =
  let paragraph =
    Paragraph.make ~scroll:scrolled_past_the_end (text_of [ "one"; "two" ])
  in
  Alcotest.(check (list string))
    "blank"
    [ blank_line ~width:room_width; blank_line ~width:room_width ]
    (lines_of paragraph ~width:room_width ~height:short_room_height)
;;

let test_negative_scroll_is_rejected () =
  Alcotest.check_raises
    "negative"
    (Invalid_argument "Paragraph.make: negative scroll")
    (fun () ->
       ignore (Paragraph.make ~scroll:negative_scroll (text_of [ "one" ])))
;;

let test_line_count () =
  let lines = [ "one"; quick_fox; "three" ] in
  let without_wrapping = Paragraph.make (text_of lines) in
  let with_wrapping = Paragraph.make ~wrap:Wrap.Word (text_of lines) in
  let wrapped_row_count = 4 in
  Alcotest.(check int)
    "one row per line"
    (List.length lines)
    (Paragraph.line_count without_wrapping ~width:wrapping_room_width);
  Alcotest.(check int)
    "rows after wrapping"
    wrapped_row_count
    (Paragraph.line_count with_wrapping ~width:wrapping_room_width)
;;

let fit_to_width ~width text =
  let cut =
    if String.length text > width then String.sub text Int.zero width else text
  in
  cut ^ blank_line ~width:(width - String.length cut)
;;

let property_left_aligned_text_fills_the_rows_in_order =
  QCheck2.Test.make
    ~name:"left-aligned lines fill the rows in order, cut to the width"
    ~print:QCheck2.Print.(list string)
    Generators.text_lines
    (fun lines ->
       let paragraph = Paragraph.make (text_of lines) in
       let rendered =
         lines_of
           paragraph
           ~width:Generators.screen_width
           ~height:Generators.screen_height
       in
       let expected =
         List.init Generators.screen_height (fun row ->
           match List.nth_opt lines row with
           | Some line -> fit_to_width ~width:Generators.screen_width line
           | None -> blank_line ~width:Generators.screen_width)
       in
       rendered = expected)
;;

let property_drawing_stays_inside_the_area =
  QCheck2.Test.make
    ~name:"a paragraph only changes cells inside its area"
    ~print:
      QCheck2.Print.(
        quad
          (list string)
          Generators.print_alignment
          Generators.print_rect
          Generators.print_edits)
    QCheck2.Gen.(
      quad
        Generators.text_lines
        Generators.alignment
        Generators.rect
        Generators.edits)
    (fun (lines, alignment, region, edits) ->
       let untouched = Support.build_buffer edits in
       let drawn = Support.build_buffer edits in
       Paragraph.widget (Paragraph.make ~alignment (text_of lines)) region drawn;
       Support.same_cells_outside region untouched drawn)
;;

let () =
  Alcotest.run
    "Paragraph"
    [ ( "text"
      , [ Alcotest.test_case "rows" `Quick test_lines_go_on_separate_rows
        ; Alcotest.test_case "alignment" `Quick test_paragraph_alignment
        ; Alcotest.test_case
            "line alignment wins"
            `Quick
            test_line_alignment_overrides_the_paragraph
        ; Alcotest.test_case
            "wide characters"
            `Quick
            test_wide_characters_are_centered_by_columns
        ; Alcotest.test_case "long lines" `Quick test_long_lines_are_cut
        ; Alcotest.test_case
            "rows beyond the bottom"
            `Quick
            test_rows_beyond_the_bottom_are_left_out
        ] )
    ; ( "wrapping and scrolling"
      , [ Alcotest.test_case
            "wrapping"
            `Quick
            test_wrapping_breaks_long_lines_into_rows
        ; Alcotest.test_case "scroll" `Quick test_scrolling_hides_the_first_rows
        ; Alcotest.test_case
            "scroll after wrapping"
            `Quick
            test_scrolling_counts_rows_after_wrapping
        ; Alcotest.test_case
            "scroll past the end"
            `Quick
            test_scrolling_past_the_end_shows_nothing
        ; Alcotest.test_case
            "negative scroll"
            `Quick
            test_negative_scroll_is_rejected
        ; Alcotest.test_case "line count" `Quick test_line_count
        ] )
    ; ( "block and style"
      , [ Alcotest.test_case
            "inside the block"
            `Quick
            test_text_stays_inside_the_block
        ; Alcotest.test_case "style" `Quick test_style_covers_the_text_area_only
        ; Alcotest.test_case "empty area" `Quick test_empty_area_draws_nothing
        ] )
    ; ( "properties"
      , List.map
          QCheck_alcotest.to_alcotest
          [ property_left_aligned_text_fills_the_rows_in_order
          ; property_drawing_stays_inside_the_area
          ] )
    ]
;;
