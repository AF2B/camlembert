module Block = Camlembert.Block
module Buffer = Camlembert.Buffer
module Cell = Camlembert.Cell
module Color = Camlembert.Color
module Frame = Camlembert.Frame
module Item_list = Camlembert.Item_list
module Line = Camlembert.Line
module Rect = Camlembert.Rect
module Selection = Camlembert.Selection
module Style = Camlembert.Style
module Terminal = Camlembert.Terminal
module Test_backend = Camlembert.Test_backend

let origin = 0
let list_width = 10
let list_height = 4
let short_list_height = 3
let framed_list_height = 5
let first_row = 0
let second_row = 1
let last_item_of_five = 4
let beyond_the_list = 99
let scrolled_offset = 2
let marker = "> "
let items = List.map Line.of_string [ "apple"; "banana"; "cherry" ]

let five_items =
  List.map Line.of_string [ "one"; "two"; "three"; "four"; "five" ]
;;

let highlight = Style.make ~foreground:Color.Yellow ()
let area ~width ~height = Rect.make ~column:origin ~row:origin ~width ~height

let render list selection ~width ~height =
  let region = area ~width ~height in
  let buffer = Buffer.create region in
  Item_list.widget list region buffer selection;
  buffer
;;

let lines_of list selection ~width ~height =
  Buffer.to_lines (render list selection ~width ~height)
;;

let selection_on position =
  let selection = Selection.create () in
  Selection.select selection position;
  selection
;;

let style_at buffer ~column ~row = (Buffer.get buffer ~column ~row).Cell.style
let blank_line = String.make list_width ' '

let test_items_go_on_separate_rows () =
  Alcotest.(check (list string))
    "rows"
    [ "apple     "; "banana    "; "cherry    "; blank_line ]
    (lines_of
       (Item_list.make items)
       (Selection.create ())
       ~width:list_width
       ~height:list_height)
;;

let test_the_symbol_marks_the_selected_row_and_every_row_leaves_room () =
  Alcotest.(check (list string))
    "rows"
    [ "  apple   "; "> banana  "; "  cherry  "; blank_line ]
    (lines_of
       (Item_list.make ~highlight_symbol:marker items)
       (selection_on (Some second_row))
       ~width:list_width
       ~height:list_height)
;;

let test_no_room_is_left_for_the_symbol_while_nothing_is_selected () =
  Alcotest.(check (list string))
    "rows"
    [ "apple     "; "banana    "; "cherry    "; blank_line ]
    (lines_of
       (Item_list.make ~highlight_symbol:marker items)
       (Selection.create ())
       ~width:list_width
       ~height:list_height)
;;

let test_the_highlight_covers_the_whole_selected_row () =
  let buffer =
    render
      (Item_list.make ~highlight_style:highlight items)
      (selection_on (Some second_row))
      ~width:list_width
      ~height:list_height
  in
  let last_column = pred list_width in
  Alcotest.(check bool)
    "end of the selected row"
    true
    (Style.equal
       highlight
       (style_at buffer ~column:last_column ~row:second_row));
  Alcotest.(check bool)
    "another row"
    true
    (Style.equal Style.default (style_at buffer ~column:origin ~row:first_row))
;;

let test_the_list_scrolls_to_show_the_selection () =
  let selection = selection_on (Some last_item_of_five) in
  Alcotest.(check (list string))
    "last rows"
    [ "three     "; "four      "; "five      " ]
    (lines_of
       (Item_list.make five_items)
       selection
       ~width:list_width
       ~height:short_list_height);
  Alcotest.(check int) "offset" scrolled_offset (Selection.offset selection)
;;

let test_the_scrolling_stays_while_the_selection_stays_in_view () =
  let selection = selection_on (Some last_item_of_five) in
  let list = Item_list.make five_items in
  ignore (lines_of list selection ~width:list_width ~height:short_list_height);
  Selection.select selection (Some (pred last_item_of_five));
  Alcotest.(check (list string))
    "same window"
    [ "three     "; "four      "; "five      " ]
    (lines_of list selection ~width:list_width ~height:short_list_height)
;;

let test_a_selection_beyond_the_list_goes_to_the_last_item () =
  let selection = selection_on (Some beyond_the_list) in
  ignore
    (lines_of
       (Item_list.make items)
       selection
       ~width:list_width
       ~height:list_height);
  Alcotest.(check (option int))
    "last item"
    (Some (pred (List.length items)))
    (Selection.selected selection)
;;

let test_an_empty_list_has_no_selection () =
  let selection = selection_on (Some second_row) in
  ignore
    (lines_of
       (Item_list.make [])
       selection
       ~width:list_width
       ~height:list_height);
  Alcotest.(check (option int))
    "nothing selected"
    None
    (Selection.selected selection)
;;

let test_long_items_are_cut_after_the_symbol () =
  let long_items = [ Line.of_string "abcdefghijklmnop" ] in
  Alcotest.(check (list string))
    "cut"
    [ "> abcdefgh" ]
    (lines_of
       (Item_list.make ~highlight_symbol:marker long_items)
       (selection_on (Some first_row))
       ~width:list_width
       ~height:Int.one)
;;

let test_a_block_frames_the_list_and_scrolling_uses_what_is_inside () =
  let block = Block.make ~borders:Block.all_sides () in
  let selection = selection_on (Some last_item_of_five) in
  Alcotest.(check (list string))
    "framed"
    [ "┌────────┐"; "│three   │"; "│four    │"; "│five    │"; "└────────┘" ]
    (lines_of
       (Item_list.make ~block five_items)
       selection
       ~width:list_width
       ~height:framed_list_height)
;;

let test_drawing_through_a_terminal () =
  let test_backend =
    Test_backend.create ~width:list_width ~height:list_height
  in
  let terminal = Terminal.create (Test_backend.backend test_backend) in
  let selection = selection_on (Some first_row) in
  let list = Item_list.make ~highlight_symbol:marker items in
  Terminal.draw terminal (fun frame ->
    Frame.render_stateful_widget
      frame
      (Item_list.widget list)
      ~area:(Frame.area frame)
      selection);
  Alcotest.(check string)
    "first row"
    "> apple   "
    (List.hd (Test_backend.to_lines test_backend))
;;

let property_drawing_stays_inside_the_area =
  QCheck2.Test.make
    ~name:"a list only changes cells inside its area"
    ~print:
      QCheck2.Print.(
        triple (option int) Generators.print_rect Generators.print_edits)
    QCheck2.Gen.(
      triple
        (option (int_range Int.zero last_item_of_five))
        Generators.rect
        Generators.edits)
    (fun (selected, region, edits) ->
       let list = Item_list.make ~highlight_symbol:marker five_items in
       Support.leaves_the_outside_untouched
         (fun region buffer ->
            Item_list.widget list region buffer (selection_on selected))
         ~region
         ~edits)
;;

let () =
  Alcotest.run
    "Item_list"
    [ ( "rows"
      , [ Alcotest.test_case "rows" `Quick test_items_go_on_separate_rows
        ; Alcotest.test_case
            "symbol"
            `Quick
            test_the_symbol_marks_the_selected_row_and_every_row_leaves_room
        ; Alcotest.test_case
            "no symbol room without selection"
            `Quick
            test_no_room_is_left_for_the_symbol_while_nothing_is_selected
        ; Alcotest.test_case
            "highlight"
            `Quick
            test_the_highlight_covers_the_whole_selected_row
        ; Alcotest.test_case
            "cut"
            `Quick
            test_long_items_are_cut_after_the_symbol
        ; Alcotest.test_case
            "block"
            `Quick
            test_a_block_frames_the_list_and_scrolling_uses_what_is_inside
        ] )
    ; ( "selection"
      , [ Alcotest.test_case
            "scrolls"
            `Quick
            test_the_list_scrolls_to_show_the_selection
        ; Alcotest.test_case
            "scrolling stays"
            `Quick
            test_the_scrolling_stays_while_the_selection_stays_in_view
        ; Alcotest.test_case
            "beyond the list"
            `Quick
            test_a_selection_beyond_the_list_goes_to_the_last_item
        ; Alcotest.test_case
            "empty list"
            `Quick
            test_an_empty_list_has_no_selection
        ; Alcotest.test_case
            "through a terminal"
            `Quick
            test_drawing_through_a_terminal
        ] )
    ; ( "properties"
      , [ QCheck_alcotest.to_alcotest property_drawing_stays_inside_the_area ] )
    ]
;;
