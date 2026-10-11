module Alignment = Camlembert.Alignment
module Buffer = Camlembert.Buffer
module Cell = Camlembert.Cell
module Color = Camlembert.Color
module Grapheme = Camlembert.Grapheme
module Line = Camlembert.Line
module Rect = Camlembert.Rect
module Span = Camlembert.Span
module Style = Camlembert.Style
module Text = Camlembert.Text

let origin = 0
let room_width = 10
let content_width = 4
let free_columns = room_width - content_width
let half_of_free_columns = free_columns / 2
let oversized_content_width = 15
let ascii_greeting = "hello"
let wide_greeting = "\u{6F22}\u{5B57}"
let wide_greeting_width = 4
let clipped_width = 3
let canvas_width = 8
let canvas_height = 1
let blank_character = ' '

let canvas =
  Rect.make ~column:origin ~row:origin ~width:canvas_width ~height:canvas_height
;;

let red_style = Style.make ~foreground:Color.Red ()
let blue_background = Style.make ~background:Color.Blue ()

let render_line line ~max_width =
  let buffer = Buffer.create canvas in
  Line.render line buffer ~column:origin ~row:origin ~max_width;
  buffer
;;

let first_row buffer = List.hd (Buffer.to_lines buffer)

let padded text =
  text ^ String.make (canvas_width - Grapheme.width text) blank_character
;;

let test_left_alignment_has_no_offset () =
  Alcotest.(check int)
    "left"
    origin
    (Alignment.offset
       Alignment.Left
       ~available:room_width
       ~content:content_width)
;;

let test_center_alignment_splits_the_free_columns () =
  Alcotest.(check int)
    "center"
    half_of_free_columns
    (Alignment.offset
       Alignment.Center
       ~available:room_width
       ~content:content_width)
;;

let test_right_alignment_pushes_the_content_to_the_end () =
  Alcotest.(check int)
    "right"
    free_columns
    (Alignment.offset
       Alignment.Right
       ~available:room_width
       ~content:content_width)
;;

let test_content_that_does_not_fit_has_no_offset () =
  List.iter
    (fun alignment ->
       Alcotest.(check int)
         "no offset"
         origin
         (Alignment.offset
            alignment
            ~available:room_width
            ~content:oversized_content_width))
    [ Alignment.Left; Alignment.Center; Alignment.Right ]
;;

let test_span_width_counts_columns () =
  Alcotest.(check int)
    "ascii"
    (String.length ascii_greeting)
    (Span.width (Span.make ascii_greeting));
  Alcotest.(check int)
    "wide"
    wide_greeting_width
    (Span.width (Span.make wide_greeting))
;;

let test_line_width_sums_its_spans () =
  let line = Line.make [ Span.make ascii_greeting; Span.make wide_greeting ] in
  Alcotest.(check int)
    "sum"
    (String.length ascii_greeting + wide_greeting_width)
    (Line.width line)
;;

let test_line_from_string_has_one_span () =
  let line = Line.of_string ~alignment:Alignment.Right ascii_greeting in
  Alcotest.(check int) "spans" Int.one (List.length line.Line.spans);
  Alcotest.(check bool)
    "alignment"
    true
    (line.Line.alignment = Some Alignment.Right)
;;

let test_render_draws_spans_one_after_another () =
  let line =
    Line.make
      [ Span.make ~style:red_style "ab"; Span.make ~style:blue_background "cd" ]
  in
  let buffer = render_line line ~max_width:canvas_width in
  let style_at column = (Buffer.get buffer ~column ~row:origin).Cell.style in
  let second_span_column = String.length "ab" in
  Alcotest.(check string) "text" (padded "abcd") (first_row buffer);
  Alcotest.(check bool)
    "first span style"
    true
    (Style.equal red_style (style_at origin));
  Alcotest.(check bool)
    "second span style"
    true
    (Style.equal blue_background (style_at second_span_column))
;;

let test_render_cuts_the_line_at_max_width () =
  let line = Line.of_string "abcdef" in
  let buffer = render_line line ~max_width:clipped_width in
  Alcotest.(check string) "clipped" (padded "abc") (first_row buffer)
;;

let test_text_has_one_line_per_line_feed () =
  let expected_lines = [ "first"; "second"; "third" ] in
  Alcotest.(check int)
    "lines"
    (List.length expected_lines)
    (List.length (Text.of_string (String.concat "\n" expected_lines)))
;;

let test_text_with_trailing_line_feed_ends_with_an_empty_line () =
  let lines = Text.of_string "a\n" in
  Alcotest.(check int) "lines" 2 (List.length lines);
  Alcotest.(check int)
    "last line is empty"
    origin
    (Line.width (List.nth lines Int.one))
;;

let property_aligned_content_stays_inside_the_room =
  QCheck2.Test.make
    ~name:"aligned content that fits stays inside the room"
    ~print:QCheck2.Print.(pair int int)
    QCheck2.Gen.(
      let* content = int_range Int.zero room_width in
      let+ available = int_range content (content + room_width) in
      available, content)
    (fun (available, content) ->
       List.for_all
         (fun alignment ->
            let offset = Alignment.offset alignment ~available ~content in
            offset >= Int.zero && offset + content <= available)
         [ Alignment.Left; Alignment.Center; Alignment.Right ])
;;

let () =
  Alcotest.run
    "Text"
    [ ( "alignment"
      , [ Alcotest.test_case "left" `Quick test_left_alignment_has_no_offset
        ; Alcotest.test_case
            "center"
            `Quick
            test_center_alignment_splits_the_free_columns
        ; Alcotest.test_case
            "right"
            `Quick
            test_right_alignment_pushes_the_content_to_the_end
        ; Alcotest.test_case
            "content that does not fit"
            `Quick
            test_content_that_does_not_fit_has_no_offset
        ] )
    ; ( "span and line"
      , [ Alcotest.test_case "span width" `Quick test_span_width_counts_columns
        ; Alcotest.test_case "line width" `Quick test_line_width_sums_its_spans
        ; Alcotest.test_case
            "of_string"
            `Quick
            test_line_from_string_has_one_span
        ; Alcotest.test_case
            "render spans"
            `Quick
            test_render_draws_spans_one_after_another
        ; Alcotest.test_case
            "render clipping"
            `Quick
            test_render_cuts_the_line_at_max_width
        ] )
    ; ( "text"
      , [ Alcotest.test_case
            "line feeds"
            `Quick
            test_text_has_one_line_per_line_feed
        ; Alcotest.test_case
            "trailing line feed"
            `Quick
            test_text_with_trailing_line_feed_ends_with_an_empty_line
        ] )
    ; ( "properties"
      , [ QCheck_alcotest.to_alcotest
            property_aligned_content_stays_inside_the_room
        ] )
    ]
;;
