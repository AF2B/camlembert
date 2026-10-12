module Backend = Camlembert.Backend
module Buffer = Camlembert.Buffer
module Cell = Camlembert.Cell
module Position = Camlembert.Position
module Test_backend = Camlembert.Test_backend

let screen_width = 6
let screen_height = 3
let first_column = 0
let first_row = 0
let second_row = 1
let last_row = 2
let last_column = 5
let third_column = 2
let wide_cell_end_column = 2
let far_away = 100
let all_rows_and_more = 10
let two_rows = 2
let smaller_width = 4
let smaller_height = 2
let wide_symbol = "\u{6F22}"
let blank_row = String.make screen_width ' '
let row_of letter = String.make screen_width letter
let padded text = text ^ String.make (screen_width - String.length text) ' '

let position_testable =
  Alcotest.testable
    (fun formatter { Position.column; row } ->
       Format.fprintf formatter "(%d, %d)" column row)
    ( = )
;;

let at ~column ~row = { Position.column; row }

let new_test_backend () =
  Test_backend.create ~width:screen_width ~height:screen_height
;;

(* A fake terminal whose three rows hold the letters a, b and c, with the cursor
   at the end of the last row. *)
let filled_test_backend () =
  let test_backend = new_test_backend () in
  Test_backend.print_line test_backend (row_of 'a');
  Test_backend.print_line test_backend (row_of 'b');
  Test_backend.print test_backend (row_of 'c');
  test_backend
;;

let change ~column ~row symbol : Buffer.change =
  { column; row; cell = Cell.make symbol }
;;

let check_cursor_position description expected test_backend =
  Alcotest.check
    position_testable
    description
    expected
    (Test_backend.cursor_position test_backend)
;;

let test_a_new_terminal_is_blank_with_a_hidden_cursor_at_the_corner () =
  let test_backend = new_test_backend () in
  Alcotest.(check (list string))
    "screen"
    (List.init screen_height (Fun.const blank_row))
    (Test_backend.to_lines test_backend);
  Alcotest.(check (list string))
    "history"
    []
    (Test_backend.history test_backend);
  Alcotest.(check (option position_testable))
    "hidden"
    None
    (Test_backend.cursor test_backend);
  check_cursor_position
    "at the corner"
    (at ~column:first_column ~row:first_row)
    test_backend
;;

let test_printing_moves_the_cursor_after_the_text () =
  let test_backend = new_test_backend () in
  Test_backend.print test_backend "ab";
  check_cursor_position
    "after the text"
    (at ~column:third_column ~row:first_row)
    test_backend;
  Test_backend.print_line test_backend "cd";
  Alcotest.(check (list string))
    "first row"
    [ padded "abcd"; blank_row; blank_row ]
    (Test_backend.to_lines test_backend);
  check_cursor_position
    "start of the next row"
    (at ~column:first_column ~row:second_row)
    test_backend
;;

let test_a_line_on_the_last_row_scrolls_the_screen () =
  let test_backend = filled_test_backend () in
  Test_backend.print_line test_backend "";
  Alcotest.(check (list string))
    "screen"
    [ row_of 'b'; row_of 'c'; blank_row ]
    (Test_backend.to_lines test_backend);
  Alcotest.(check (list string))
    "history"
    [ row_of 'a' ]
    (Test_backend.history test_backend);
  check_cursor_position
    "start of the last row"
    (at ~column:first_column ~row:last_row)
    test_backend
;;

let test_drawing_leaves_the_cursor_after_the_last_cell () =
  let test_backend = new_test_backend () in
  let backend = Test_backend.backend test_backend in
  backend.Backend.draw
    [ change ~column:first_column ~row:second_row "a"
    ; change ~column:(succ first_column) ~row:second_row "b"
    ];
  check_cursor_position
    "after b"
    (at ~column:third_column ~row:second_row)
    test_backend;
  backend.Backend.draw
    [ change ~column:first_column ~row:first_row wide_symbol ];
  check_cursor_position
    "after a wide cell"
    (at ~column:wide_cell_end_column ~row:first_row)
    test_backend;
  backend.Backend.draw [ change ~column:last_column ~row:last_row "z" ];
  check_cursor_position
    "waits at the last column"
    (at ~column:last_column ~row:last_row)
    test_backend;
  backend.Backend.draw [];
  check_cursor_position
    "nothing drawn, nothing moved"
    (at ~column:last_column ~row:last_row)
    test_backend
;;

let test_clear_from_blanks_to_the_end_in_reading_order () =
  let test_backend = filled_test_backend () in
  let backend = Test_backend.backend test_backend in
  backend.Backend.clear_from (at ~column:third_column ~row:second_row);
  Alcotest.(check (list string))
    "screen"
    [ row_of 'a'; padded "bb"; blank_row ]
    (Test_backend.to_lines test_backend);
  check_cursor_position
    "on the first blanked cell"
    (at ~column:third_column ~row:second_row)
    test_backend
;;

let test_scrolling_keeps_the_rows_that_leave_and_the_column () =
  let test_backend = filled_test_backend () in
  let backend = Test_backend.backend test_backend in
  backend.Backend.move_cursor (at ~column:third_column ~row:first_row);
  backend.Backend.scroll_up two_rows;
  Alcotest.(check (list string))
    "screen"
    [ row_of 'c'; blank_row; blank_row ]
    (Test_backend.to_lines test_backend);
  Alcotest.(check (list string))
    "history"
    [ row_of 'a'; row_of 'b' ]
    (Test_backend.history test_backend);
  check_cursor_position
    "last row, same column"
    (at ~column:third_column ~row:last_row)
    test_backend
;;

let test_scrolling_more_rows_than_the_screen_has () =
  let test_backend = filled_test_backend () in
  let backend = Test_backend.backend test_backend in
  backend.Backend.scroll_up all_rows_and_more;
  Alcotest.(check (list string))
    "screen"
    (List.init screen_height (Fun.const blank_row))
    (Test_backend.to_lines test_backend);
  Alcotest.(check (list string))
    "history"
    [ row_of 'a'; row_of 'b'; row_of 'c' ]
    (Test_backend.history test_backend)
;;

let test_moving_does_not_show_the_cursor_but_setting_does () =
  let test_backend = new_test_backend () in
  let backend = Test_backend.backend test_backend in
  let target = at ~column:third_column ~row:second_row in
  backend.Backend.move_cursor target;
  Alcotest.(check (option position_testable))
    "moved, still hidden"
    None
    (Test_backend.cursor test_backend);
  check_cursor_position "moved" target test_backend;
  backend.Backend.set_cursor (Some target);
  Alcotest.(check (option position_testable))
    "shown"
    (Some target)
    (Test_backend.cursor test_backend);
  backend.Backend.set_cursor None;
  Alcotest.(check (option position_testable))
    "hidden again"
    None
    (Test_backend.cursor test_backend);
  check_cursor_position "still there" target test_backend
;;

let test_the_cursor_stays_on_the_screen () =
  let test_backend = new_test_backend () in
  let backend = Test_backend.backend test_backend in
  backend.Backend.move_cursor (at ~column:far_away ~row:far_away);
  check_cursor_position
    "last cell"
    (at ~column:last_column ~row:last_row)
    test_backend
;;

let test_resizing_blanks_the_screen_but_keeps_the_history () =
  let test_backend = filled_test_backend () in
  Test_backend.print_line test_backend "";
  Test_backend.resize test_backend ~width:smaller_width ~height:smaller_height;
  Alcotest.(check (list string))
    "screen"
    (List.init smaller_height (Fun.const (String.make smaller_width ' ')))
    (Test_backend.to_lines test_backend);
  Alcotest.(check (list string))
    "history"
    [ row_of 'a' ]
    (Test_backend.history test_backend);
  check_cursor_position
    "inside the smaller screen"
    (at ~column:first_column ~row:(pred smaller_height))
    test_backend
;;

let property_scrolling_loses_nothing =
  QCheck2.Test.make
    ~name:"history and screen together keep every row that was scrolled"
    ~print:QCheck2.Print.(pair Generators.print_edits int)
    QCheck2.Gen.(
      pair
        Generators.edits
        (int_range Int.zero (Generators.screen_height + two_rows)))
    (fun (edits, rows) ->
       let test_backend =
         Test_backend.create
           ~width:Generators.screen_width
           ~height:Generators.screen_height
       in
       let backend = Test_backend.backend test_backend in
       let blank =
         Buffer.create (Buffer.area (Test_backend.screen test_backend))
       in
       backend.Backend.draw
         (Buffer.diff ~previous:blank ~next:(Support.build_buffer edits));
       let before =
         Test_backend.history test_backend @ Test_backend.to_lines test_backend
       in
       backend.Backend.scroll_up rows;
       let scrolled = Int.min rows Generators.screen_height in
       let blank_rows =
         List.init
           scrolled
           (Fun.const (String.make Generators.screen_width ' '))
       in
       Test_backend.history test_backend @ Test_backend.to_lines test_backend
       = before @ blank_rows)
;;

let () =
  Alcotest.run
    "Test backend"
    [ ( "screen"
      , [ Alcotest.test_case
            "new terminal"
            `Quick
            test_a_new_terminal_is_blank_with_a_hidden_cursor_at_the_corner
        ; Alcotest.test_case
            "printing"
            `Quick
            test_printing_moves_the_cursor_after_the_text
        ; Alcotest.test_case
            "line on the last row"
            `Quick
            test_a_line_on_the_last_row_scrolls_the_screen
        ; Alcotest.test_case
            "drawing"
            `Quick
            test_drawing_leaves_the_cursor_after_the_last_cell
        ; Alcotest.test_case
            "clear from"
            `Quick
            test_clear_from_blanks_to_the_end_in_reading_order
        ; Alcotest.test_case
            "resize"
            `Quick
            test_resizing_blanks_the_screen_but_keeps_the_history
        ] )
    ; ( "scrolling"
      , [ Alcotest.test_case
            "keeps rows and column"
            `Quick
            test_scrolling_keeps_the_rows_that_leave_and_the_column
        ; Alcotest.test_case
            "more rows than the screen"
            `Quick
            test_scrolling_more_rows_than_the_screen_has
        ] )
    ; ( "cursor"
      , [ Alcotest.test_case
            "move and set"
            `Quick
            test_moving_does_not_show_the_cursor_but_setting_does
        ; Alcotest.test_case
            "stays on the screen"
            `Quick
            test_the_cursor_stays_on_the_screen
        ] )
    ; ( "properties"
      , [ QCheck_alcotest.to_alcotest property_scrolling_loses_nothing ] )
    ]
;;
