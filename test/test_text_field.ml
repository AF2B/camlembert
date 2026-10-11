module Block = Camlembert.Block
module Buffer = Camlembert.Buffer
module Cell = Camlembert.Cell
module Color = Camlembert.Color
module Event = Camlembert.Event
module Frame = Camlembert.Frame
module Line_edit = Camlembert.Line_edit
module Position = Camlembert.Position
module Rect = Camlembert.Rect
module Style = Camlembert.Style
module Terminal = Camlembert.Terminal
module Test_backend = Camlembert.Test_backend
module Text_field = Camlembert.Text_field

let origin = 0
let field_width = 10
let narrow_width = 5
let one_row = 1
let framed_height = 3
let border_thickness = 1
let long_text = "abcdefghij"

(* The cursor needs the cell after the last character, so one fewer fits. *)
let tail_of_long_text = "ghij"
let placeholder = "name"
let greeting = "hello"
let green = Style.make ~foreground:Color.Green ()
let dimmed = Style.make ~modifiers:[ Style.Dim ] ()

let position_testable =
  Alcotest.testable
    (fun formatter { Position.column; row } ->
       Format.fprintf formatter "(%d, %d)" column row)
    ( = )
;;

let area ~width ~height = Rect.make ~column:origin ~row:origin ~width ~height

let render field edit ~width ~height =
  let region = area ~width ~height in
  let buffer = Buffer.create region in
  Text_field.widget field region buffer edit;
  buffer
;;

let first_line field edit ~width =
  List.hd (Buffer.to_lines (render field edit ~width ~height:one_row))
;;

let pad text ~width = text ^ String.make (width - String.length text) ' '

let test_the_text_is_shown_from_the_left () =
  Alcotest.(check string)
    "text"
    (pad greeting ~width:field_width)
    (first_line
       (Text_field.make ())
       (Line_edit.create ~text:greeting ())
       ~width:field_width)
;;

let test_the_placeholder_is_shown_while_the_line_is_empty () =
  let field = Text_field.make ~placeholder () in
  let buffer =
    render field (Line_edit.create ()) ~width:field_width ~height:one_row
  in
  Alcotest.(check string)
    "placeholder"
    (pad placeholder ~width:field_width)
    (List.hd (Buffer.to_lines buffer));
  Alcotest.(check bool)
    "dimmed by default"
    true
    (Style.equal
       dimmed
       (Buffer.get buffer ~column:origin ~row:origin).Cell.style);
  Alcotest.(check string)
    "gone once there is text"
    (pad greeting ~width:field_width)
    (first_line field (Line_edit.create ~text:greeting ()) ~width:field_width)
;;

let test_the_placeholder_style_can_be_changed () =
  let field = Text_field.make ~placeholder ~placeholder_style:green () in
  let buffer =
    render field (Line_edit.create ()) ~width:field_width ~height:one_row
  in
  Alcotest.(check bool)
    "green"
    true
    (Style.equal
       green
       (Buffer.get buffer ~column:origin ~row:origin).Cell.style)
;;

let test_a_long_line_shows_the_part_around_the_cursor () =
  Alcotest.(check string)
    "tail"
    (pad tail_of_long_text ~width:narrow_width)
    (first_line
       (Text_field.make ())
       (Line_edit.create ~text:long_text ())
       ~width:narrow_width)
;;

let test_a_block_frames_the_field () =
  let block = Block.make ~borders:Block.all_sides () in
  let lines =
    Buffer.to_lines
      (render
         (Text_field.make ~block ())
         (Line_edit.create ~text:greeting ())
         ~width:(field_width + (2 * border_thickness))
         ~height:framed_height)
  in
  Alcotest.(check string)
    "inside the frame"
    ("\u{2502}" ^ pad greeting ~width:field_width ^ "\u{2502}")
    (List.nth lines border_thickness)
;;

let test_the_cursor_goes_after_the_text () =
  let field = Text_field.make () in
  let edit = Line_edit.create ~text:greeting () in
  Alcotest.check
    position_testable
    "end of the text"
    { Position.column = String.length greeting; row = origin }
    (Text_field.cursor_position
       field
       edit
       ~area:(area ~width:field_width ~height:one_row));
  Line_edit.move_to_start edit;
  Alcotest.check
    position_testable
    "start of the text"
    { Position.column = origin; row = origin }
    (Text_field.cursor_position
       field
       edit
       ~area:(area ~width:field_width ~height:one_row))
;;

let test_the_cursor_stays_inside_the_field_when_the_line_is_longer () =
  let field = Text_field.make () in
  let edit = Line_edit.create ~text:long_text () in
  Alcotest.check
    position_testable
    "last column"
    { Position.column = pred narrow_width; row = origin }
    (Text_field.cursor_position
       field
       edit
       ~area:(area ~width:narrow_width ~height:one_row))
;;

let test_the_cursor_follows_the_block () =
  let block = Block.make ~borders:Block.all_sides () in
  let field = Text_field.make ~block () in
  let edit = Line_edit.create ~text:greeting () in
  Alcotest.check
    position_testable
    "inside the frame"
    { Position.column = border_thickness + String.length greeting
    ; row = border_thickness
    }
    (Text_field.cursor_position
       field
       edit
       ~area:
         (area
            ~width:(field_width + (2 * border_thickness))
            ~height:framed_height))
;;

let test_typing_in_a_field_through_a_terminal () =
  let test_backend = Test_backend.create ~width:field_width ~height:one_row in
  let terminal = Terminal.create (Test_backend.backend test_backend) in
  let field = Text_field.make ~placeholder () in
  let edit = Line_edit.create () in
  let draw () =
    Terminal.draw terminal (fun frame ->
      let region = Frame.area frame in
      Frame.render_stateful_widget
        frame
        (Text_field.widget field)
        ~area:region
        edit;
      Frame.set_cursor
        frame
        (Text_field.cursor_position field edit ~area:region))
  in
  draw ();
  Alcotest.(check (list string))
    "placeholder"
    [ pad placeholder ~width:field_width ]
    (Test_backend.to_lines test_backend);
  List.iter
    (fun letter ->
       ignore (Line_edit.handle edit (Event.key_press (Event.Char letter))))
    [ "h"; "i" ];
  draw ();
  Alcotest.(check (list string))
    "typed text"
    [ pad "hi" ~width:field_width ]
    (Test_backend.to_lines test_backend);
  Alcotest.(check (option position_testable))
    "cursor after the text"
    (Some { Position.column = String.length "hi"; row = origin })
    (Test_backend.cursor test_backend)
;;

let property_drawing_stays_inside_the_area =
  QCheck2.Test.make
    ~name:"a field only changes cells inside its area"
    ~print:
      QCheck2.Print.(triple string Generators.print_rect Generators.print_edits)
    QCheck2.Gen.(
      triple Generators.printable_ascii_text Generators.rect Generators.edits)
    (fun (text, region, edits) ->
       let field = Text_field.make ~placeholder () in
       Support.leaves_the_outside_untouched
         (fun region buffer ->
            Text_field.widget field region buffer (Line_edit.create ~text ()))
         ~region
         ~edits)
;;

let () =
  Alcotest.run
    "Text_field"
    [ ( "drawing"
      , [ Alcotest.test_case "text" `Quick test_the_text_is_shown_from_the_left
        ; Alcotest.test_case
            "placeholder"
            `Quick
            test_the_placeholder_is_shown_while_the_line_is_empty
        ; Alcotest.test_case
            "placeholder style"
            `Quick
            test_the_placeholder_style_can_be_changed
        ; Alcotest.test_case
            "long line"
            `Quick
            test_a_long_line_shows_the_part_around_the_cursor
        ; Alcotest.test_case "block" `Quick test_a_block_frames_the_field
        ] )
    ; ( "cursor"
      , [ Alcotest.test_case
            "after the text"
            `Quick
            test_the_cursor_goes_after_the_text
        ; Alcotest.test_case
            "long line"
            `Quick
            test_the_cursor_stays_inside_the_field_when_the_line_is_longer
        ; Alcotest.test_case "block" `Quick test_the_cursor_follows_the_block
        ; Alcotest.test_case
            "through a terminal"
            `Quick
            test_typing_in_a_field_through_a_terminal
        ] )
    ; ( "properties"
      , [ QCheck_alcotest.to_alcotest property_drawing_stays_inside_the_area ] )
    ]
;;
