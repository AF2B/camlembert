module Ansi = Camlembert_unix.Ansi
module Buffer = Camlembert.Buffer
module Cell = Camlembert.Cell
module Color = Camlembert.Color
module Rect = Camlembert.Rect
module Style = Camlembert.Style

let escape = "\027"
let first_column = 0
let first_row = 0
let late_column = 2
let second_row = 1
let red_channel = 10
let green_channel = 20
let blue_channel = 30
let extended_palette_index = 200
let wide_symbol = "\u{6F22}"
let red_foreground = Style.make ~foreground:Color.Red ()
let blue_foreground = Style.make ~foreground:Color.Blue ()

let change ?(style = Style.default) ~column ~row symbol : Buffer.change =
  { column; row; cell = Cell.make ~style symbol }
;;

let reset = escape ^ "[0m"
let fourth_row = 3
let last_row_of_a_short_screen = 23
let two_rows = 2
let report_row = 12
let report_column = 40
let huge_number = "99999999999999999999999"

let position_testable =
  Alcotest.testable
    (fun formatter { Camlembert.Position.column; row } ->
       Format.fprintf formatter "(%d, %d)" column row)
    ( = )
;;

let test_mode_switches () =
  Alcotest.(check string)
    "enter alternate screen"
    (escape ^ "[?1049h")
    Ansi.enter_alternate_screen;
  Alcotest.(check string)
    "leave alternate screen"
    (escape ^ "[?1049l")
    Ansi.leave_alternate_screen;
  Alcotest.(check string)
    "enable mouse"
    (escape ^ "[?1000h" ^ escape ^ "[?1002h" ^ escape ^ "[?1006h")
    Ansi.enable_mouse;
  Alcotest.(check string)
    "disable mouse"
    (escape ^ "[?1006l" ^ escape ^ "[?1002l" ^ escape ^ "[?1000l")
    Ansi.disable_mouse;
  Alcotest.(check string)
    "enable bracketed paste"
    (escape ^ "[?2004h")
    Ansi.enable_bracketed_paste;
  Alcotest.(check string)
    "disable bracketed paste"
    (escape ^ "[?2004l")
    Ansi.disable_bracketed_paste;
  Alcotest.(check string) "hide cursor" (escape ^ "[?25l") Ansi.hide_cursor;
  Alcotest.(check string) "show cursor" (escape ^ "[?25h") Ansi.show_cursor;
  Alcotest.(check string)
    "clear screen"
    (escape ^ "[2J" ^ escape ^ "[H")
    Ansi.clear_screen;
  Alcotest.(check string) "reset style" reset Ansi.reset_style
;;

let test_move_cursor_counts_from_one () =
  Alcotest.(check string)
    "top-left corner"
    (escape ^ "[1;1H")
    (Ansi.move_cursor ~column:first_column ~row:first_row);
  Alcotest.(check string)
    "third column, second row"
    (escape ^ "[2;3H")
    (Ansi.move_cursor ~column:late_column ~row:second_row)
;;

let test_cursor_is_moved_before_it_is_shown () =
  Alcotest.(check string)
    "shown"
    (escape ^ "[2;3H" ^ escape ^ "[?25h")
    (Ansi.encode_cursor
       (Some { Camlembert.Position.column = late_column; row = second_row }));
  Alcotest.(check string) "hidden" (escape ^ "[?25l") (Ansi.encode_cursor None)
;;

let test_clear_from_moves_and_erases_to_the_end_of_the_screen () =
  Alcotest.(check string)
    "from a cell"
    (escape ^ "[4;3H" ^ escape ^ "[0J")
    (Ansi.clear_from ~column:late_column ~row:fourth_row)
;;

let test_scrolling_prints_line_feeds_on_the_last_row () =
  Alcotest.(check string)
    "two rows"
    (escape ^ "[24;1H\n\n")
    (Ansi.scroll_up ~rows:two_rows ~last_row:last_row_of_a_short_screen);
  Alcotest.(check string)
    "nothing to scroll"
    ""
    (Ansi.scroll_up ~rows:Int.zero ~last_row:last_row_of_a_short_screen);
  Alcotest.(check string)
    "negative"
    ""
    (Ansi.scroll_up
       ~rows:(Int.neg two_rows)
       ~last_row:last_row_of_a_short_screen)
;;

let test_the_cursor_position_request () =
  Alcotest.(check string)
    "device status report"
    (escape ^ "[6n")
    Ansi.request_cursor_position
;;

let report ~row ~column = Printf.sprintf "%s[%d;%dR" escape row column
let reported_position ~column ~row = { Camlembert.Position.column; row }

let test_the_cursor_report_counts_from_one () =
  Alcotest.(check (option position_testable))
    "report"
    (Some
       (reported_position ~column:(pred report_column) ~row:(pred report_row)))
    (Ansi.find_cursor_report (report ~row:report_row ~column:report_column));
  Alcotest.(check (option position_testable))
    "top-left corner"
    (Some (reported_position ~column:first_column ~row:first_row))
    (Ansi.find_cursor_report (report ~row:Int.one ~column:Int.one))
;;

let test_the_cursor_report_is_found_among_other_input () =
  let expected =
    Some (reported_position ~column:(pred report_column) ~row:(pred report_row))
  in
  let report_text = report ~row:report_row ~column:report_column in
  Alcotest.(check (option position_testable))
    "typed before"
    expected
    (Ansi.find_cursor_report ("ls\r" ^ report_text));
  Alcotest.(check (option position_testable))
    "key sequence before"
    expected
    (Ansi.find_cursor_report (escape ^ "[A" ^ report_text));
  Alcotest.(check (option position_testable))
    "typed after"
    expected
    (Ansi.find_cursor_report (report_text ^ "x"))
;;

let test_input_without_a_complete_report_has_none () =
  List.iter
    (fun (description, input) ->
       Alcotest.(check (option position_testable))
         description
         None
         (Ansi.find_cursor_report input))
    [ "empty", ""
    ; "plain text", "hello"
    ; "cut after the row", escape ^ "[12"
    ; "cut after the separator", escape ^ "[12;"
    ; "cut before the final byte", escape ^ "[12;40"
    ; "another final byte", escape ^ "[12;40H"
    ; "no row", escape ^ "[;40R"
    ; "no column", escape ^ "[12;R"
    ; "huge numbers", escape ^ "[" ^ huge_number ^ ";1R"
    ]
;;

let test_select_style () =
  let rgb =
    Color.Rgb { red = red_channel; green = green_channel; blue = blue_channel }
  in
  let indexed = Color.Indexed extended_palette_index in
  Alcotest.(check string)
    "nothing to select"
    ""
    (Ansi.select_style Style.default);
  Alcotest.(check string)
    "effects"
    (escape ^ "[1;4m")
    (Ansi.select_style
       (Style.make ~modifiers:[ Style.Underlined; Style.Bold ] ()));
  Alcotest.(check string)
    "named foreground"
    (escape ^ "[38;5;1m")
    (Ansi.select_style red_foreground);
  Alcotest.(check string)
    "indexed background"
    (escape ^ "[48;5;200m")
    (Ansi.select_style (Style.make ~background:indexed ()));
  Alcotest.(check string)
    "true color foreground"
    (escape ^ "[38;2;10;20;30m")
    (Ansi.select_style (Style.make ~foreground:rgb ()));
  Alcotest.(check string)
    "default foreground"
    (escape ^ "[39m")
    (Ansi.select_style (Style.make ~foreground:Color.Reset ()));
  Alcotest.(check string)
    "everything together"
    (escape ^ "[1;38;5;1;48;5;4m")
    (Ansi.select_style
       (Style.make
          ~foreground:Color.Red
          ~background:Color.Blue
          ~modifiers:[ Style.Bold ]
          ()))
;;

let test_no_changes_send_nothing () =
  Alcotest.(check string) "empty" "" (Ansi.encode_changes [])
;;

let test_single_change () =
  Alcotest.(check string)
    "move, write, reset"
    (escape ^ "[2;3Hx" ^ reset)
    (Ansi.encode_changes [ change ~column:late_column ~row:second_row "x" ])
;;

let test_adjacent_cells_share_one_cursor_move () =
  let next_column = succ first_column in
  Alcotest.(check string)
    "one move"
    (escape ^ "[1;1Hab" ^ reset)
    (Ansi.encode_changes
       [ change ~column:first_column ~row:first_row "a"
       ; change ~column:next_column ~row:first_row "b"
       ])
;;

let test_gaps_and_new_rows_move_the_cursor () =
  Alcotest.(check string)
    "gap in a row"
    (escape ^ "[1;1Ha" ^ escape ^ "[1;3Hb" ^ reset)
    (Ansi.encode_changes
       [ change ~column:first_column ~row:first_row "a"
       ; change ~column:late_column ~row:first_row "b"
       ]);
  Alcotest.(check string)
    "next row"
    (escape ^ "[1;1Ha" ^ escape ^ "[2;2Hb" ^ reset)
    (Ansi.encode_changes
       [ change ~column:first_column ~row:first_row "a"
       ; change ~column:(succ first_column) ~row:second_row "b"
       ])
;;

let test_wide_characters_advance_the_cursor_by_their_width () =
  Alcotest.(check string)
    "no move after a wide character"
    (escape ^ "[1;1H" ^ wide_symbol ^ "a" ^ reset)
    (Ansi.encode_changes
       [ change ~column:first_column ~row:first_row wide_symbol
       ; change ~column:late_column ~row:first_row "a"
       ])
;;

let test_styles_are_selected_only_when_they_change () =
  let next_column = succ first_column in
  let after_next_column = succ next_column in
  Alcotest.(check string)
    "default, red, default"
    (escape ^ "[1;1Ha" ^ escape ^ "[38;5;1mb" ^ reset ^ "c" ^ reset)
    (Ansi.encode_changes
       [ change ~column:first_column ~row:first_row "a"
       ; change ~style:red_foreground ~column:next_column ~row:first_row "b"
       ; change ~column:after_next_column ~row:first_row "c"
       ]);
  Alcotest.(check string)
    "red, red, blue"
    (escape
     ^ "[1;1H"
     ^ escape
     ^ "[38;5;1mab"
     ^ reset
     ^ escape
     ^ "[38;5;4mc"
     ^ reset)
    (Ansi.encode_changes
       [ change ~style:red_foreground ~column:first_column ~row:first_row "a"
       ; change ~style:red_foreground ~column:next_column ~row:first_row "b"
       ; change
           ~style:blue_foreground
           ~column:after_next_column
           ~row:first_row
           "c"
       ])
;;

let property_replaying_the_output_shows_the_next_frame =
  QCheck2.Test.make
    ~name:"a terminal replaying the output shows the next frame"
    ~print:QCheck2.Print.(pair Generators.print_edits Generators.print_edits)
    QCheck2.Gen.(pair Generators.edits Generators.edits)
    (fun (previous_edits, next_edits) ->
       let previous = Support.build_buffer previous_edits in
       let next = Support.build_buffer next_edits in
       let screen = Support.build_buffer previous_edits in
       Ansi_emulator.replay
         ~screen
         (Ansi.encode_changes (Buffer.diff ~previous ~next));
       let shows_the_same_cell ~column ~row =
         Cell.equal
           (Ansi_emulator.normalize_cell (Buffer.get screen ~column ~row))
           (Ansi_emulator.normalize_cell (Buffer.get next ~column ~row))
       in
       let same = ref true in
       Rect.iter_positions Generators.screen (fun ~column ~row ->
         if not (shows_the_same_cell ~column ~row) then same := false);
       !same)
;;

let () =
  Alcotest.run
    "Ansi"
    [ ( "sequences"
      , [ Alcotest.test_case "mode switches" `Quick test_mode_switches
        ; Alcotest.test_case
            "move cursor"
            `Quick
            test_move_cursor_counts_from_one
        ; Alcotest.test_case
            "cursor"
            `Quick
            test_cursor_is_moved_before_it_is_shown
        ; Alcotest.test_case "select style" `Quick test_select_style
        ; Alcotest.test_case
            "clear from"
            `Quick
            test_clear_from_moves_and_erases_to_the_end_of_the_screen
        ; Alcotest.test_case
            "scroll up"
            `Quick
            test_scrolling_prints_line_feeds_on_the_last_row
        ] )
    ; ( "cursor report"
      , [ Alcotest.test_case "request" `Quick test_the_cursor_position_request
        ; Alcotest.test_case
            "counts from one"
            `Quick
            test_the_cursor_report_counts_from_one
        ; Alcotest.test_case
            "among other input"
            `Quick
            test_the_cursor_report_is_found_among_other_input
        ; Alcotest.test_case
            "incomplete"
            `Quick
            test_input_without_a_complete_report_has_none
        ] )
    ; ( "encode changes"
      , [ Alcotest.test_case "no changes" `Quick test_no_changes_send_nothing
        ; Alcotest.test_case "single change" `Quick test_single_change
        ; Alcotest.test_case
            "adjacent cells"
            `Quick
            test_adjacent_cells_share_one_cursor_move
        ; Alcotest.test_case
            "gaps and new rows"
            `Quick
            test_gaps_and_new_rows_move_the_cursor
        ; Alcotest.test_case
            "wide characters"
            `Quick
            test_wide_characters_advance_the_cursor_by_their_width
        ; Alcotest.test_case
            "style changes"
            `Quick
            test_styles_are_selected_only_when_they_change
        ] )
    ; ( "properties"
      , [ QCheck_alcotest.to_alcotest
            property_replaying_the_output_shows_the_next_frame
        ] )
    ]
;;
