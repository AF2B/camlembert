module Backend = Camlembert.Backend
module Buffer = Camlembert.Buffer
module Frame = Camlembert.Frame
module Position = Camlembert.Position
module Rect = Camlembert.Rect
module Style = Camlembert.Style
module Terminal = Camlembert.Terminal
module Test_backend = Camlembert.Test_backend
module Widget = Camlembert.Widget

let screen_width = 10
let screen_height = 8
let first_column = 0
let viewport_height = 3
let greeting = "hello"
let changed_greeting = "jello"
let half_typed_text = "abc"
let oversized_height = 100
let two_lines = 2
let four_lines = 4
let more_lines_than_the_screen = 12
let first_log = "log A"
let second_log = "log B"
let inserted_row_after_one_log = 3
let seven_lines = 7
let scrolled_rows = 2
let seven_lines_viewport_top = 5
let one_changed_cell = 1
let resized_width = 12
let resized_height = 6
let cursor_column = 2
let cursor_offset = 1
let shifted_cursor_row = 2
let no_rows = 0
let blank_character = ' '

let padded text =
  text ^ String.make (screen_width - String.length text) blank_character
;;

let printed_line index = Printf.sprintf "line %d" index
let printed_lines count = List.init count printed_line

let position_testable =
  Alcotest.testable
    (fun formatter { Position.column; row } ->
       Format.fprintf formatter "(%d, %d)" column row)
    ( = )
;;

let rect_testable = Alcotest.testable Rect.pp Rect.equal

(* A widget that writes [text] in the top-left corner of its area. *)
let text_widget text : Widget.t =
  fun area buffer ->
  Buffer.set_string
    buffer
    ~column:area.Rect.column
    ~row:area.Rect.row
    ~max_width:area.Rect.width
    ~style:Style.default
    text
;;

let draw_text terminal text =
  Terminal.draw terminal (fun frame ->
    Frame.render_widget frame (text_widget text) ~area:(Frame.area frame))
;;

(* A fake terminal where a shell has printed [count] lines, so the cursor waits
   at the start of the line after them. *)
let backend_after ~lines:count =
  let test_backend =
    Test_backend.create ~width:screen_width ~height:screen_height
  in
  List.iter (Test_backend.print_line test_backend) (printed_lines count);
  test_backend
;;

let inline_terminal test_backend ~height =
  Terminal.create
    ~viewport:(Terminal.Inline height)
    (Test_backend.backend test_backend)
;;

let rows_of test_backend ~count =
  List.filteri
    (fun index _ -> index < count)
    (Test_backend.to_lines test_backend)
;;

let check_cursor_position description expected test_backend =
  Alcotest.check
    position_testable
    description
    expected
    (Test_backend.cursor_position test_backend)
;;

let test_the_viewport_starts_on_the_line_of_the_cursor () =
  let test_backend = backend_after ~lines:two_lines in
  let terminal = inline_terminal test_backend ~height:viewport_height in
  Alcotest.check
    rect_testable
    "area"
    (Rect.make
       ~column:first_column
       ~row:two_lines
       ~width:screen_width
       ~height:viewport_height)
    (Terminal.area terminal);
  Alcotest.(check (list string))
    "what was printed stays"
    (List.map padded (printed_lines two_lines))
    (rows_of test_backend ~count:two_lines);
  Alcotest.(check (list string))
    "nothing scrolled"
    []
    (Test_backend.history test_backend)
;;

let test_the_screen_scrolls_when_there_is_no_room_below_the_cursor () =
  let test_backend = backend_after ~lines:seven_lines in
  let terminal = inline_terminal test_backend ~height:viewport_height in
  Alcotest.check
    rect_testable
    "area at the bottom"
    (Rect.make
       ~column:first_column
       ~row:seven_lines_viewport_top
       ~width:screen_width
       ~height:viewport_height)
    (Terminal.area terminal);
  Alcotest.(check (list string))
    "the first lines went to the history"
    (List.map padded (printed_lines scrolled_rows))
    (Test_backend.history test_backend);
  Alcotest.(check (list string))
    "the rest is still above the viewport"
    (List.map
       padded
       (List.filteri
          (fun index _ -> index >= scrolled_rows)
          (printed_lines seven_lines)))
    (rows_of test_backend ~count:seven_lines_viewport_top)
;;

let test_a_cursor_in_the_middle_of_a_line_keeps_that_line () =
  let test_backend = backend_after ~lines:Int.zero in
  Test_backend.print test_backend half_typed_text;
  let terminal = inline_terminal test_backend ~height:viewport_height in
  Alcotest.(check int)
    "starts on the next row"
    Int.one
    (Terminal.area terminal).Rect.row;
  Alcotest.(check string)
    "the text is kept"
    (padded half_typed_text)
    (List.hd (Test_backend.to_lines test_backend))
;;

let test_a_cursor_in_the_middle_of_the_last_line_scrolls_one_more_row () =
  let test_backend = backend_after ~lines:seven_lines in
  Test_backend.print test_backend half_typed_text;
  let terminal = inline_terminal test_backend ~height:two_lines in
  Alcotest.(check int)
    "viewport at the bottom"
    (screen_height - two_lines)
    (Terminal.area terminal).Rect.row;
  Alcotest.(check string)
    "the text is kept, above the viewport"
    (padded half_typed_text)
    (List.nth
       (Test_backend.to_lines test_backend)
       (screen_height - two_lines - Int.one))
;;

let test_a_viewport_taller_than_the_screen_takes_the_whole_screen () =
  let test_backend = backend_after ~lines:two_lines in
  let terminal = inline_terminal test_backend ~height:oversized_height in
  Alcotest.check
    rect_testable
    "whole screen"
    (Rect.make
       ~column:first_column
       ~row:Int.zero
       ~width:screen_width
       ~height:screen_height)
    (Terminal.area terminal);
  Alcotest.(check (list string))
    "the lines went to the history"
    (List.map padded (printed_lines two_lines))
    (Test_backend.history test_backend)
;;

let test_a_viewport_needs_at_least_one_row () =
  Alcotest.check_raises
    "no rows"
    (Invalid_argument
       "Terminal.create: an inline viewport needs at least one row")
    (fun () ->
       ignore (inline_terminal (backend_after ~lines:two_lines) ~height:no_rows))
;;

let test_frames_are_drawn_inside_the_viewport () =
  let test_backend = backend_after ~lines:two_lines in
  let terminal = inline_terminal test_backend ~height:viewport_height in
  draw_text terminal greeting;
  Alcotest.(check (list string))
    "screen"
    (List.map padded (printed_lines two_lines)
     @ [ padded greeting ]
     @ List.init (screen_height - two_lines - Int.one) (Fun.const (padded "")))
    (Test_backend.to_lines test_backend)
;;

let test_only_the_changed_cells_of_the_viewport_are_sent () =
  let test_backend = backend_after ~lines:two_lines in
  let terminal = inline_terminal test_backend ~height:viewport_height in
  draw_text terminal greeting;
  let drawn_after_first_frame = Test_backend.drawn_cells test_backend in
  draw_text terminal greeting;
  Alcotest.(check int)
    "same frame, nothing sent"
    drawn_after_first_frame
    (Test_backend.drawn_cells test_backend);
  draw_text terminal changed_greeting;
  Alcotest.(check int)
    "one changed cell"
    (drawn_after_first_frame + one_changed_cell)
    (Test_backend.drawn_cells test_backend)
;;

let test_a_hidden_cursor_waits_on_the_first_row_of_the_viewport () =
  let test_backend = backend_after ~lines:two_lines in
  let terminal = inline_terminal test_backend ~height:viewport_height in
  draw_text terminal greeting;
  Alcotest.(check (option position_testable))
    "hidden"
    None
    (Test_backend.cursor test_backend);
  check_cursor_position
    "parked"
    { Position.column = first_column; row = two_lines }
    test_backend
;;

let test_a_requested_cursor_is_shown_where_the_frame_asks () =
  let test_backend = backend_after ~lines:two_lines in
  let terminal = inline_terminal test_backend ~height:viewport_height in
  let requested =
    { Position.column = cursor_column; row = two_lines + cursor_offset }
  in
  Terminal.draw terminal (fun frame -> Frame.set_cursor frame requested);
  Alcotest.(check (option position_testable))
    "shown"
    (Some requested)
    (Test_backend.cursor test_backend)
;;

let test_clearing_blanks_only_the_viewport () =
  let test_backend = backend_after ~lines:two_lines in
  let terminal = inline_terminal test_backend ~height:viewport_height in
  draw_text terminal greeting;
  Terminal.clear terminal;
  Alcotest.(check (list string))
    "the lines above stay, the viewport is blank"
    (List.map padded (printed_lines two_lines)
     @ List.init (screen_height - two_lines) (Fun.const (padded "")))
    (Test_backend.to_lines test_backend);
  let drawn_before = Test_backend.drawn_cells test_backend in
  draw_text terminal greeting;
  Alcotest.(check int)
    "the next draw sends the whole frame"
    (drawn_before + String.length greeting)
    (Test_backend.drawn_cells test_backend)
;;

let test_a_resize_places_the_viewport_again_from_the_cursor () =
  let test_backend = backend_after ~lines:four_lines in
  let terminal = inline_terminal test_backend ~height:viewport_height in
  draw_text terminal greeting;
  Test_backend.resize test_backend ~width:resized_width ~height:resized_height;
  draw_text terminal greeting;
  let area = Terminal.area terminal in
  Alcotest.(check int) "new width" resized_width area.Rect.width;
  Alcotest.(check int)
    "pushed up to fit the shorter screen"
    (resized_height - viewport_height)
    area.Rect.row;
  Alcotest.(check string)
    "the frame is drawn there"
    (greeting
     ^ String.make (resized_width - String.length greeting) blank_character)
    (List.nth (Test_backend.to_lines test_backend) area.Rect.row)
;;

let test_a_resize_keeps_the_cursor_on_its_row_of_the_viewport () =
  let test_backend = backend_after ~lines:two_lines in
  let terminal = inline_terminal test_backend ~height:viewport_height in
  let requested =
    { Position.column = cursor_column; row = two_lines + cursor_offset }
  in
  Terminal.draw terminal (fun frame -> Frame.set_cursor frame requested);
  Test_backend.resize test_backend ~width:resized_width ~height:screen_height;
  (Test_backend.backend test_backend).Backend.move_cursor
    { Position.column = cursor_column; row = shifted_cursor_row };
  Terminal.draw terminal (fun frame ->
    Frame.set_cursor
      frame
      { Position.column = cursor_column; row = shifted_cursor_row });
  Alcotest.(check int)
    "viewport follows the cursor"
    (shifted_cursor_row - cursor_offset)
    (Terminal.area terminal).Rect.row
;;

let test_finishing_leaves_the_cursor_below_the_viewport () =
  let test_backend = backend_after ~lines:two_lines in
  let terminal = inline_terminal test_backend ~height:viewport_height in
  draw_text terminal greeting;
  Terminal.finish terminal;
  check_cursor_position
    "start of the line below"
    { Position.column = first_column; row = two_lines + viewport_height }
    test_backend;
  Alcotest.(check (list string))
    "nothing scrolled"
    []
    (Test_backend.history test_backend)
;;

let test_finishing_a_viewport_at_the_bottom_scrolls_one_row () =
  let test_backend = backend_after ~lines:seven_lines in
  let terminal = inline_terminal test_backend ~height:viewport_height in
  draw_text terminal greeting;
  let history_before = List.length (Test_backend.history test_backend) in
  Terminal.finish terminal;
  Alcotest.(check int)
    "one more row in the history"
    (history_before + Int.one)
    (List.length (Test_backend.history test_backend));
  check_cursor_position
    "start of the last row"
    { Position.column = first_column; row = pred screen_height }
    test_backend;
  Alcotest.(check string)
    "the last row of the screen is blank"
    (padded "")
    (List.nth (Test_backend.to_lines test_backend) (pred screen_height))
;;

let test_finishing_a_full_screen_does_nothing () =
  let test_backend =
    Test_backend.create ~width:screen_width ~height:screen_height
  in
  let terminal = Terminal.create (Test_backend.backend test_backend) in
  draw_text terminal greeting;
  let cursor_before = Test_backend.cursor_position test_backend in
  Terminal.finish terminal;
  check_cursor_position "unchanged" cursor_before test_backend;
  Alcotest.(check (list string))
    "nothing scrolled"
    []
    (Test_backend.history test_backend)
;;

(* Prints the lines above the viewport, one per row. *)
let insert terminal lines =
  Terminal.insert_before terminal ~height:(List.length lines) (fun frame ->
    List.iteri
      (fun row line ->
         Buffer.set_string
           (Frame.buffer frame)
           ~column:first_column
           ~row
           ~max_width:screen_width
           ~style:Style.default
           line)
      lines)
;;

let blank_rows count = List.init count (Fun.const (padded ""))

let test_inserted_lines_go_above_the_viewport_and_push_it_down () =
  let test_backend = backend_after ~lines:two_lines in
  let terminal = inline_terminal test_backend ~height:viewport_height in
  draw_text terminal greeting;
  insert terminal [ first_log ];
  Alcotest.(check (list string))
    "screen"
    (List.map padded (printed_lines two_lines)
     @ [ padded first_log; padded greeting ]
     @ blank_rows (screen_height - inserted_row_after_one_log - Int.one))
    (Test_backend.to_lines test_backend);
  Alcotest.(check int)
    "the viewport moved down"
    inserted_row_after_one_log
    (Terminal.area terminal).Rect.row;
  Alcotest.(check (list string))
    "nothing scrolled"
    []
    (Test_backend.history test_backend)
;;

let test_the_screen_scrolls_when_the_viewport_is_at_the_bottom () =
  let test_backend = backend_after ~lines:seven_lines in
  let terminal = inline_terminal test_backend ~height:viewport_height in
  draw_text terminal greeting;
  insert terminal [ first_log; second_log ];
  let lines = printed_lines seven_lines in
  Alcotest.(check (list string))
    "history"
    (List.map padded (List.filteri (fun index _ -> index < four_lines) lines))
    (Test_backend.history test_backend);
  Alcotest.(check (list string))
    "screen"
    (List.map padded (List.filteri (fun index _ -> index >= four_lines) lines)
     @ [ padded first_log; padded second_log; padded greeting ]
     @ blank_rows (viewport_height - Int.one))
    (Test_backend.to_lines test_backend);
  Alcotest.(check int)
    "the viewport stayed at the bottom"
    (screen_height - viewport_height)
    (Terminal.area terminal).Rect.row
;;

let test_lines_inserted_one_after_the_other_keep_their_order () =
  let test_backend = backend_after ~lines:two_lines in
  let terminal = inline_terminal test_backend ~height:viewport_height in
  draw_text terminal greeting;
  insert terminal [ first_log ];
  insert terminal [ second_log ];
  Alcotest.(check (list string))
    "above the viewport"
    (List.map padded (printed_lines two_lines @ [ first_log; second_log ]))
    (rows_of test_backend ~count:(two_lines + two_lines))
;;

let test_the_next_draw_sends_only_what_changed_since_the_insertion () =
  let test_backend = backend_after ~lines:two_lines in
  let terminal = inline_terminal test_backend ~height:viewport_height in
  draw_text terminal greeting;
  insert terminal [ first_log ];
  let drawn_after_insertion = Test_backend.drawn_cells test_backend in
  draw_text terminal greeting;
  Alcotest.(check int)
    "same frame, nothing sent"
    drawn_after_insertion
    (Test_backend.drawn_cells test_backend);
  draw_text terminal changed_greeting;
  Alcotest.(check int)
    "one changed cell"
    (drawn_after_insertion + one_changed_cell)
    (Test_backend.drawn_cells test_backend)
;;

let test_the_cursor_follows_the_viewport_when_lines_are_inserted () =
  let test_backend = backend_after ~lines:two_lines in
  let terminal = inline_terminal test_backend ~height:viewport_height in
  let requested =
    { Position.column = cursor_column; row = two_lines + cursor_offset }
  in
  Terminal.draw terminal (fun frame -> Frame.set_cursor frame requested);
  insert terminal [ first_log ];
  Alcotest.(check (option position_testable))
    "shown, one row lower"
    (Some { requested with Position.row = requested.Position.row + Int.one })
    (Test_backend.cursor test_backend);
  let hidden_backend = backend_after ~lines:two_lines in
  let hidden_terminal =
    inline_terminal hidden_backend ~height:viewport_height
  in
  draw_text hidden_terminal greeting;
  insert hidden_terminal [ first_log ];
  check_cursor_position
    "hidden, on the first row of the viewport"
    { Position.column = first_column; row = inserted_row_after_one_log }
    hidden_backend
;;

let test_more_lines_than_the_screen_can_show_all_reach_the_history () =
  let test_backend = backend_after ~lines:two_lines in
  let terminal = inline_terminal test_backend ~height:viewport_height in
  draw_text terminal greeting;
  let logs = List.init more_lines_than_the_screen (Printf.sprintf "log %d") in
  insert terminal logs;
  Alcotest.(check (list string))
    "everything printed so far, in order"
    (List.map padded (printed_lines two_lines @ logs))
    (Test_backend.history test_backend
     @ rows_of test_backend ~count:(Terminal.area terminal).Rect.row);
  Alcotest.(check string)
    "the frame is still there"
    (padded greeting)
    (List.nth
       (Test_backend.to_lines test_backend)
       (Terminal.area terminal).Rect.row)
;;

let test_inserting_nothing_changes_nothing () =
  let test_backend = backend_after ~lines:two_lines in
  let terminal = inline_terminal test_backend ~height:viewport_height in
  draw_text terminal greeting;
  let lines_before = Test_backend.to_lines test_backend in
  let drawn_before = Test_backend.drawn_cells test_backend in
  Terminal.insert_before terminal ~height:Int.zero (fun _ -> ());
  Alcotest.(check (list string))
    "same screen"
    lines_before
    (Test_backend.to_lines test_backend);
  Alcotest.(check int)
    "nothing sent"
    drawn_before
    (Test_backend.drawn_cells test_backend)
;;

let test_the_lines_are_composed_on_a_region_as_wide_as_the_viewport () =
  let test_backend = backend_after ~lines:two_lines in
  let terminal = inline_terminal test_backend ~height:viewport_height in
  let seen = ref None in
  Terminal.insert_before terminal ~height:two_lines (fun frame ->
    seen := Some (Frame.area frame));
  Alcotest.(check (option rect_testable))
    "area"
    (Some
       (Rect.make
          ~column:first_column
          ~row:Int.zero
          ~width:screen_width
          ~height:two_lines))
    !seen
;;

let test_only_an_inline_viewport_has_room_above_it () =
  let full_screen =
    Terminal.create
      (Test_backend.backend
         (Test_backend.create ~width:screen_width ~height:screen_height))
  in
  Alcotest.check_raises
    "full screen"
    (Invalid_argument
       "Terminal.insert_before: only an inline viewport has room above it")
    (fun () -> Terminal.insert_before full_screen ~height:Int.one (fun _ -> ()));
  let inline =
    inline_terminal (backend_after ~lines:two_lines) ~height:viewport_height
  in
  Alcotest.check_raises
    "negative height"
    (Invalid_argument "Terminal.insert_before: height cannot be negative")
    (fun () ->
       Terminal.insert_before inline ~height:(Int.neg Int.one) (fun _ -> ()))
;;

let min_screen_height = 3
let max_screen_height = 10
let max_line_count = 14
let max_viewport_height = 12

let property_what_was_printed_stays_above_the_viewport =
  QCheck2.Test.make
    ~name:"the lines printed before an inline viewport stay right above it"
    ~print:QCheck2.Print.(triple int int int)
    QCheck2.Gen.(
      triple
        (int_range min_screen_height max_screen_height)
        (int_range Int.zero max_line_count)
        (int_range Int.one max_viewport_height))
    (fun (height, line_count, requested_height) ->
       let test_backend = Test_backend.create ~width:screen_width ~height in
       List.iter
         (Test_backend.print_line test_backend)
         (printed_lines line_count);
       let terminal = inline_terminal test_backend ~height:requested_height in
       let area = Terminal.area terminal in
       let above =
         Test_backend.history test_backend
         @ rows_of test_backend ~count:area.Rect.row
       in
       area.Rect.height = Int.min requested_height height
       && area.Rect.row >= Int.zero
       && Rect.bottom area <= height
       && above = List.map padded (printed_lines line_count))
;;

let max_inserted_count = 14

let property_inserted_lines_join_what_was_printed_above_the_viewport =
  QCheck2.Test.make
    ~name:"inserted lines follow the printed ones and the frame survives"
    ~print:QCheck2.Print.(quad int int int int)
    QCheck2.Gen.(
      quad
        (int_range min_screen_height max_screen_height)
        (int_range Int.zero max_line_count)
        (int_range Int.one max_viewport_height)
        (int_range Int.zero max_inserted_count))
    (fun (height, line_count, requested_height, inserted_count) ->
       let test_backend = Test_backend.create ~width:screen_width ~height in
       List.iter
         (Test_backend.print_line test_backend)
         (printed_lines line_count);
       let terminal = inline_terminal test_backend ~height:requested_height in
       draw_text terminal greeting;
       let inserted = List.init inserted_count (Printf.sprintf "log %d") in
       if inserted <> [] then insert terminal inserted;
       let area = Terminal.area terminal in
       let above =
         Test_backend.history test_backend
         @ rows_of test_backend ~count:area.Rect.row
       in
       above = List.map padded (printed_lines line_count @ inserted)
       && area.Rect.height = Int.min requested_height height
       && Rect.bottom area <= height
       && List.nth (Test_backend.to_lines test_backend) area.Rect.row
          = padded greeting
       && Test_backend.cursor_position test_backend
          = { Position.column = first_column; row = area.Rect.row })
;;

let property_a_resized_viewport_stays_on_the_screen =
  QCheck2.Test.make
    ~name:"after a resize the viewport lies inside the new screen"
    ~print:QCheck2.Print.(quad int int int int)
    QCheck2.Gen.(
      quad
        (int_range Int.zero max_line_count)
        (int_range Int.one max_viewport_height)
        (int_range min_screen_height max_screen_height)
        (int_range Int.one screen_width))
    (fun (line_count, requested_height, new_height, new_width) ->
       let test_backend = backend_after ~lines:line_count in
       let terminal = inline_terminal test_backend ~height:requested_height in
       draw_text terminal greeting;
       Test_backend.resize test_backend ~width:new_width ~height:new_height;
       draw_text terminal greeting;
       let area = Terminal.area terminal in
       area.Rect.width = new_width
       && area.Rect.height = Int.min requested_height new_height
       && area.Rect.row >= Int.zero
       && Rect.bottom area <= new_height)
;;

let () =
  Alcotest.run
    "Inline viewport"
    [ ( "placement"
      , [ Alcotest.test_case
            "line of the cursor"
            `Quick
            test_the_viewport_starts_on_the_line_of_the_cursor
        ; Alcotest.test_case
            "scrolls when short of room"
            `Quick
            test_the_screen_scrolls_when_there_is_no_room_below_the_cursor
        ; Alcotest.test_case
            "cursor mid-line"
            `Quick
            test_a_cursor_in_the_middle_of_a_line_keeps_that_line
        ; Alcotest.test_case
            "cursor mid-line at the bottom"
            `Quick
            test_a_cursor_in_the_middle_of_the_last_line_scrolls_one_more_row
        ; Alcotest.test_case
            "taller than the screen"
            `Quick
            test_a_viewport_taller_than_the_screen_takes_the_whole_screen
        ; Alcotest.test_case
            "at least one row"
            `Quick
            test_a_viewport_needs_at_least_one_row
        ] )
    ; ( "drawing"
      , [ Alcotest.test_case
            "inside the viewport"
            `Quick
            test_frames_are_drawn_inside_the_viewport
        ; Alcotest.test_case
            "changed cells only"
            `Quick
            test_only_the_changed_cells_of_the_viewport_are_sent
        ; Alcotest.test_case
            "clear"
            `Quick
            test_clearing_blanks_only_the_viewport
        ] )
    ; ( "cursor"
      , [ Alcotest.test_case
            "parked"
            `Quick
            test_a_hidden_cursor_waits_on_the_first_row_of_the_viewport
        ; Alcotest.test_case
            "requested"
            `Quick
            test_a_requested_cursor_is_shown_where_the_frame_asks
        ] )
    ; ( "resize"
      , [ Alcotest.test_case
            "from the cursor"
            `Quick
            test_a_resize_places_the_viewport_again_from_the_cursor
        ; Alcotest.test_case
            "cursor row kept"
            `Quick
            test_a_resize_keeps_the_cursor_on_its_row_of_the_viewport
        ] )
    ; ( "finish"
      , [ Alcotest.test_case
            "below the viewport"
            `Quick
            test_finishing_leaves_the_cursor_below_the_viewport
        ; Alcotest.test_case
            "at the bottom"
            `Quick
            test_finishing_a_viewport_at_the_bottom_scrolls_one_row
        ; Alcotest.test_case
            "full screen"
            `Quick
            test_finishing_a_full_screen_does_nothing
        ] )
    ; ( "insert before"
      , [ Alcotest.test_case
            "pushes the viewport down"
            `Quick
            test_inserted_lines_go_above_the_viewport_and_push_it_down
        ; Alcotest.test_case
            "scrolls at the bottom"
            `Quick
            test_the_screen_scrolls_when_the_viewport_is_at_the_bottom
        ; Alcotest.test_case
            "keeps the order"
            `Quick
            test_lines_inserted_one_after_the_other_keep_their_order
        ; Alcotest.test_case
            "next draw"
            `Quick
            test_the_next_draw_sends_only_what_changed_since_the_insertion
        ; Alcotest.test_case
            "cursor"
            `Quick
            test_the_cursor_follows_the_viewport_when_lines_are_inserted
        ; Alcotest.test_case
            "more than the screen"
            `Quick
            test_more_lines_than_the_screen_can_show_all_reach_the_history
        ; Alcotest.test_case
            "nothing"
            `Quick
            test_inserting_nothing_changes_nothing
        ; Alcotest.test_case
            "composing region"
            `Quick
            test_the_lines_are_composed_on_a_region_as_wide_as_the_viewport
        ; Alcotest.test_case
            "only inline"
            `Quick
            test_only_an_inline_viewport_has_room_above_it
        ] )
    ; ( "properties"
      , List.map
          QCheck_alcotest.to_alcotest
          [ property_what_was_printed_stays_above_the_viewport
          ; property_inserted_lines_join_what_was_printed_above_the_viewport
          ; property_a_resized_viewport_stays_on_the_screen
          ] )
    ]
;;
