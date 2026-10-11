module Block = Camlembert.Block
module Buffer = Camlembert.Buffer
module Cell = Camlembert.Cell
module Clear = Camlembert.Clear
module Color = Camlembert.Color
module Line = Camlembert.Line
module Paragraph = Camlembert.Paragraph
module Rect = Camlembert.Rect
module Style = Camlembert.Style
module Text = Camlembert.Text

let origin = 0
let oversize_factor = 2
let screen_width = 8
let screen_height = 3
let popup_column = 2
let popup_width = 4
let popup_height = 3
let filled_rows = [ "abcdefgh"; "ijklmnop"; "qrstuvwx" ]
let red_background = Style.make ~background:Color.Red ()

let screen =
  Rect.make ~column:origin ~row:origin ~width:screen_width ~height:screen_height
;;

let filled_buffer () =
  let buffer = Buffer.create screen in
  List.iteri
    (fun row text ->
       Buffer.set_string
         buffer
         ~column:origin
         ~row
         ~max_width:screen_width
         ~style:red_background
         text)
    filled_rows;
  buffer
;;

let middle =
  Rect.make ~column:popup_column ~row:Int.one ~width:popup_width ~height:Int.one
;;

let test_only_the_region_is_blanked () =
  let buffer = filled_buffer () in
  Buffer.clear buffer middle;
  Alcotest.(check (list string))
    "rows"
    [ "abcdefgh"; "ij    op"; "qrstuvwx" ]
    (Buffer.to_lines buffer)
;;

let test_the_styles_of_the_region_are_gone_too () =
  let buffer = filled_buffer () in
  Buffer.clear buffer middle;
  let inside = Buffer.get buffer ~column:popup_column ~row:Int.one in
  let outside = Buffer.get buffer ~column:origin ~row:Int.one in
  Alcotest.(check bool) "inside" true (Cell.equal Cell.empty inside);
  Alcotest.(check bool)
    "outside keeps its style"
    true
    (Style.equal red_background outside.Cell.style)
;;

let test_the_part_outside_the_buffer_is_ignored () =
  let buffer = filled_buffer () in
  let oversized =
    Rect.make
      ~column:origin
      ~row:origin
      ~width:(oversize_factor * screen_width)
      ~height:(oversize_factor * screen_height)
  in
  Buffer.clear buffer oversized;
  Alcotest.(check (list string))
    "everything blank"
    (List.map (fun text -> String.make (String.length text) ' ') filled_rows)
    (Buffer.to_lines buffer)
;;

let test_the_widget_hides_what_is_under_a_popup () =
  let buffer = filled_buffer () in
  let popup =
    Rect.make
      ~column:popup_column
      ~row:origin
      ~width:popup_width
      ~height:popup_height
  in
  let block = Block.make ~borders:Block.all_sides () in
  Clear.widget popup buffer;
  Paragraph.widget (Paragraph.make ~block (Text.of_string "")) popup buffer;
  Alcotest.(check (list string))
    "popup over the text"
    [ "ab┌──┐gh"; "ij│  │op"; "qr└──┘wx" ]
    (Buffer.to_lines buffer)
;;

let property_clearing_stays_inside_the_region =
  QCheck2.Test.make
    ~name:"clearing only changes cells inside the region"
    ~print:QCheck2.Print.(pair Generators.print_rect Generators.print_edits)
    QCheck2.Gen.(pair Generators.rect Generators.edits)
    (fun (region, edits) ->
       Support.leaves_the_outside_untouched Clear.widget ~region ~edits)
;;

let () =
  Alcotest.run
    "Clear"
    [ ( "clear"
      , [ Alcotest.test_case "region" `Quick test_only_the_region_is_blanked
        ; Alcotest.test_case
            "styles"
            `Quick
            test_the_styles_of_the_region_are_gone_too
        ; Alcotest.test_case
            "outside the buffer"
            `Quick
            test_the_part_outside_the_buffer_is_ignored
        ; Alcotest.test_case
            "popup"
            `Quick
            test_the_widget_hides_what_is_under_a_popup
        ] )
    ; ( "properties"
      , [ QCheck_alcotest.to_alcotest property_clearing_stays_inside_the_region
        ] )
    ]
;;
