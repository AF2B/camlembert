module Backend = Camlembert.Backend
module Buffer = Camlembert.Buffer
module Frame = Camlembert.Frame
module Rect = Camlembert.Rect
module Style = Camlembert.Style
module Terminal = Camlembert.Terminal
module Test_backend = Camlembert.Test_backend
module Widget = Camlembert.Widget

let screen_width = 8
let screen_height = 2
let bigger_screen_width = 12
let bigger_screen_height = 3
let greeting = "hello"
let changed_greeting = "jello"
let greeting_length = String.length greeting
let one_changed_cell = 1
let one_cell = 1
let region_column = 1
let region_row = 1
let marker = "x"
let max_frame_count = 5
let blank_character = ' '
let blank_line ~width = String.make width blank_character

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

let new_terminal () =
  let test_backend =
    Test_backend.create ~width:screen_width ~height:screen_height
  in
  test_backend, Terminal.create (Test_backend.backend test_backend)
;;

let padded text ~width = text ^ blank_line ~width:(width - String.length text)

let test_draw_shows_the_widget_on_the_backend () =
  let test_backend, terminal = new_terminal () in
  draw_text terminal greeting;
  Alcotest.(check (list string))
    "screen"
    [ padded greeting ~width:screen_width; blank_line ~width:screen_width ]
    (Test_backend.to_lines test_backend)
;;

let test_drawing_the_same_frame_again_sends_nothing () =
  let test_backend, terminal = new_terminal () in
  draw_text terminal greeting;
  let drawn_after_first_frame = Test_backend.drawn_cells test_backend in
  draw_text terminal greeting;
  Alcotest.(check int)
    "no new cells"
    drawn_after_first_frame
    (Test_backend.drawn_cells test_backend)
;;

let test_only_the_changed_cell_is_sent () =
  let test_backend, terminal = new_terminal () in
  draw_text terminal greeting;
  let drawn_after_first_frame = Test_backend.drawn_cells test_backend in
  draw_text terminal changed_greeting;
  Alcotest.(check int)
    "one new cell"
    (drawn_after_first_frame + one_changed_cell)
    (Test_backend.drawn_cells test_backend)
;;

let test_content_missing_from_the_next_frame_is_erased () =
  let test_backend, terminal = new_terminal () in
  draw_text terminal greeting;
  draw_text terminal "";
  Alcotest.(check (list string))
    "blank screen"
    [ blank_line ~width:screen_width; blank_line ~width:screen_width ]
    (Test_backend.to_lines test_backend)
;;

let test_resizing_redraws_the_whole_frame () =
  let test_backend, terminal = new_terminal () in
  draw_text terminal greeting;
  Test_backend.resize
    test_backend
    ~width:bigger_screen_width
    ~height:bigger_screen_height;
  draw_text terminal greeting;
  Alcotest.(check int)
    "terminal area width"
    bigger_screen_width
    (Terminal.area terminal).Rect.width;
  Alcotest.(check string)
    "first row"
    (padded greeting ~width:bigger_screen_width)
    (List.hd (Test_backend.to_lines test_backend))
;;

let test_clear_makes_the_next_draw_send_everything () =
  let test_backend, terminal = new_terminal () in
  draw_text terminal greeting;
  let drawn_after_first_frame = Test_backend.drawn_cells test_backend in
  Terminal.clear terminal;
  draw_text terminal greeting;
  Alcotest.(check int)
    "everything again"
    (drawn_after_first_frame + greeting_length)
    (Test_backend.drawn_cells test_backend)
;;

let test_render_widget_hands_over_the_region () =
  let test_backend, terminal = new_terminal () in
  let region =
    Rect.make
      ~column:region_column
      ~row:region_row
      ~width:one_cell
      ~height:one_cell
  in
  Terminal.draw terminal (fun frame ->
    Frame.render_widget frame (text_widget marker) ~area:region);
  let cells_after_marker = screen_width - region_column - one_cell in
  Alcotest.(check (list string))
    "drawn inside the region"
    [ blank_line ~width:screen_width
    ; blank_line ~width:region_column
      ^ marker
      ^ blank_line ~width:cells_after_marker
    ]
    (Test_backend.to_lines test_backend)
;;

let property_the_screen_always_shows_the_last_frame =
  QCheck2.Test.make
    ~name:"after every draw the screen shows exactly that frame"
    ~print:QCheck2.Print.(list Generators.print_edits)
    QCheck2.Gen.(list_size (int_range Int.one max_frame_count) Generators.edits)
    (fun frames ->
       let test_backend =
         Test_backend.create
           ~width:Generators.screen_width
           ~height:Generators.screen_height
       in
       let terminal = Terminal.create (Test_backend.backend test_backend) in
       List.for_all
         (fun edits ->
            Terminal.draw terminal (fun frame ->
              Support.write_edits (Frame.buffer frame) edits);
            Support.same_cells
              (Test_backend.screen test_backend)
              (Support.build_buffer edits))
         frames)
;;

let () =
  Alcotest.run
    "Terminal"
    [ ( "draw"
      , [ Alcotest.test_case
            "shows the widget"
            `Quick
            test_draw_shows_the_widget_on_the_backend
        ; Alcotest.test_case
            "same frame sends nothing"
            `Quick
            test_drawing_the_same_frame_again_sends_nothing
        ; Alcotest.test_case
            "only changed cells"
            `Quick
            test_only_the_changed_cell_is_sent
        ; Alcotest.test_case
            "erases missing content"
            `Quick
            test_content_missing_from_the_next_frame_is_erased
        ; Alcotest.test_case
            "render_widget region"
            `Quick
            test_render_widget_hands_over_the_region
        ] )
    ; ( "screen changes"
      , [ Alcotest.test_case
            "resize redraws everything"
            `Quick
            test_resizing_redraws_the_whole_frame
        ; Alcotest.test_case
            "clear redraws everything"
            `Quick
            test_clear_makes_the_next_draw_send_everything
        ] )
    ; ( "properties"
      , [ QCheck_alcotest.to_alcotest
            property_the_screen_always_shows_the_last_frame
        ] )
    ]
;;
