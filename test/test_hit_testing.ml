module Block = Camlembert.Block
module Buffer = Camlembert.Buffer
module Item_list = Camlembert.Item_list
module Line = Camlembert.Line
module Position = Camlembert.Position
module Rect = Camlembert.Rect
module Selection = Camlembert.Selection
module Table = Camlembert.Table
module Tabs = Camlembert.Tabs
module Constraint = Camlembert.Constraint

let origin = 0
let tabs_width = 30
let one_row = 1
let list_width = 12
let visible_rows = 3
let scrolled_offset = 2
let last_item = 4
let both_sides = 2
let framed_rows = 3
let first_title = 0
let second_title = 1
let third_title = 2

(* " One " takes columns 0 to 4, a divider column 5, " Two " columns 6 to 10
   and " Three " columns 12 to 18. *)
let first_tab_first_column = 0
let first_tab_letters_column = 2
let first_tab_last_column = 4
let first_divider_column = 5
let second_tab_first_column = 6
let second_tab_last_column = 10
let third_tab_column = 14
let past_the_last_tab = 19
let first_body_row = 1
let empty_item_row = 2
let items_in_the_short_list = 2
let item_count = 5
let border_thickness = 1
let beyond_the_area = 100
let at ~column ~row = { Position.column; row }
let titles = List.map Line.of_string [ "One"; "Two"; "Three" ]

let items =
  List.init item_count (fun index ->
    Line.of_string (Printf.sprintf "item-%d" index))
;;

let area ~width ~height = Rect.make ~column:origin ~row:origin ~width ~height
let tabs_area = area ~width:tabs_width ~height:one_row

let framed_area ~width ~height =
  area
    ~width:(width + (both_sides * border_thickness))
    ~height:(height + (both_sides * border_thickness))
;;

let block = Block.make ~borders:Block.all_sides ()

let selection_on position =
  let selection = Selection.create () in
  Selection.select selection position;
  selection
;;

let check_index message expected actual =
  Alcotest.(check (option int)) message expected actual
;;

let test_a_click_on_a_title_or_its_spaces_finds_the_title () =
  let tabs = Tabs.make titles in
  let index column =
    Tabs.index_at tabs ~area:tabs_area (at ~column ~row:origin)
  in
  check_index
    "left space of the first"
    (Some first_title)
    (index first_tab_first_column);
  check_index
    "letters of the first"
    (Some first_title)
    (index first_tab_letters_column);
  check_index
    "right space of the first"
    (Some first_title)
    (index first_tab_last_column);
  check_index "divider" None (index first_divider_column);
  check_index "second" (Some second_title) (index second_tab_first_column);
  check_index
    "last column of the second"
    (Some second_title)
    (index second_tab_last_column);
  check_index "third" (Some third_title) (index third_tab_column);
  check_index "past the last title" None (index past_the_last_tab);
  check_index "past the area" None (index beyond_the_area)
;;

let test_a_click_off_the_row_of_the_tabs_finds_nothing () =
  let tabs = Tabs.make titles in
  check_index
    "row below"
    None
    (Tabs.index_at
       tabs
       ~area:(area ~width:tabs_width ~height:framed_rows)
       (at ~column:first_tab_letters_column ~row:first_body_row))
;;

let test_the_block_moves_the_titles () =
  let tabs = Tabs.make ~block titles in
  let region = framed_area ~width:tabs_width ~height:one_row in
  check_index
    "on the border"
    None
    (Tabs.index_at
       tabs
       ~area:region
       (at ~column:first_tab_letters_column ~row:origin));
  check_index
    "inside"
    (Some first_title)
    (Tabs.index_at
       tabs
       ~area:region
       (at
          ~column:(first_tab_letters_column + border_thickness)
          ~row:border_thickness))
;;

let test_a_click_on_a_list_item_finds_it_whatever_the_scrolling () =
  let list = Item_list.make items in
  let selection = selection_on (Some last_item) in
  let region = area ~width:list_width ~height:visible_rows in
  let index row =
    Item_list.index_at list selection ~area:region (at ~column:origin ~row)
  in
  check_index "first visible row" (Some scrolled_offset) (index origin);
  check_index "last visible row" (Some last_item) (index (pred visible_rows));
  check_index "below the list area" None (index visible_rows)
;;

let test_rows_without_items_and_columns_outside_find_nothing () =
  let list =
    Item_list.make
      (List.filteri (fun index _ -> index < items_in_the_short_list) items)
  in
  let region = area ~width:list_width ~height:visible_rows in
  let selection = Selection.create () in
  check_index
    "empty row"
    None
    (Item_list.index_at
       list
       selection
       ~area:region
       (at ~column:origin ~row:empty_item_row));
  check_index
    "beyond the width"
    None
    (Item_list.index_at
       list
       selection
       ~area:region
       (at ~column:list_width ~row:origin))
;;

let test_the_block_of_a_list_is_not_an_item () =
  let list = Item_list.make ~block items in
  let selection = Selection.create () in
  let region = framed_area ~width:list_width ~height:visible_rows in
  check_index
    "border"
    None
    (Item_list.index_at
       list
       selection
       ~area:region
       (at ~column:border_thickness ~row:origin));
  check_index
    "first item"
    (Some first_title)
    (Item_list.index_at
       list
       selection
       ~area:region
       (at ~column:border_thickness ~row:border_thickness))
;;

let table_rows = List.map (fun line -> [ line ]) items
let widths = [ Constraint.Fill Int.one ]

let test_the_header_of_a_table_is_not_a_row () =
  let table = Table.make ~header:[ Line.of_string "Name" ] ~widths table_rows in
  let selection = Selection.create () in
  let region = area ~width:list_width ~height:(succ visible_rows) in
  check_index
    "header"
    None
    (Table.index_at
       table
       selection
       ~area:region
       (at ~column:origin ~row:origin));
  check_index
    "first row"
    (Some first_title)
    (Table.index_at
       table
       selection
       ~area:region
       (at ~column:origin ~row:first_body_row));
  check_index
    "last visible row"
    (Some (pred visible_rows))
    (Table.index_at
       table
       selection
       ~area:region
       (at ~column:origin ~row:visible_rows))
;;

let test_a_scrolled_table_shifts_the_rows () =
  let table = Table.make ~header:[ Line.of_string "Name" ] ~widths table_rows in
  let selection = selection_on (Some last_item) in
  let region = area ~width:list_width ~height:(succ visible_rows) in
  check_index
    "first visible row"
    (Some scrolled_offset)
    (Table.index_at
       table
       selection
       ~area:region
       (at ~column:origin ~row:first_body_row));
  check_index
    "last row"
    (Some last_item)
    (Table.index_at
       table
       selection
       ~area:region
       (at ~column:origin ~row:visible_rows))
;;

(* The text drawn on a row says which item it is, so what is drawn can be
   compared with what a click would find. *)
let drawn_item_of line =
  match String.split_on_char '-' (String.trim line) with
  | [ "item"; number ] -> int_of_string_opt number
  | _ -> None
;;

let max_item_count = 8
let max_height = 6

let scenario =
  QCheck2.Gen.(
    triple
      (int_range Int.zero max_item_count)
      (option (int_range Int.zero (pred max_item_count)))
      (int_range Int.one max_height))
;;

let print_scenario = QCheck2.Print.(triple int (option int) int)

let numbered_items count =
  List.init count (fun index -> Line.of_string (Printf.sprintf "item-%d" index))
;;

let property_a_list_click_finds_what_is_drawn =
  QCheck2.Test.make
    ~name:"a click on a list finds exactly the item drawn there"
    ~print:print_scenario
    scenario
    (fun (count, selected, height) ->
       let list = Item_list.make (numbered_items count) in
       let selection = selection_on selected in
       let region = area ~width:list_width ~height in
       let buffer = Buffer.create region in
       Item_list.widget list region buffer selection;
       List.for_all
         (fun row ->
            let drawn = drawn_item_of (List.nth (Buffer.to_lines buffer) row) in
            Item_list.index_at
              list
              selection
              ~area:region
              (at ~column:origin ~row)
            = drawn)
         (List.init height Fun.id))
;;

let property_a_table_click_finds_what_is_drawn =
  QCheck2.Test.make
    ~name:"a click on a table finds exactly the row drawn there"
    ~print:print_scenario
    scenario
    (fun (count, selected, height) ->
       let table =
         Table.make
           ~header:[ Line.of_string "Name" ]
           ~widths
           (List.map (fun line -> [ line ]) (numbered_items count))
       in
       let selection = selection_on selected in
       let region = area ~width:list_width ~height in
       let buffer = Buffer.create region in
       Table.widget table region buffer selection;
       List.for_all
         (fun row ->
            let drawn = drawn_item_of (List.nth (Buffer.to_lines buffer) row) in
            Table.index_at table selection ~area:region (at ~column:origin ~row)
            = drawn)
         (List.init height Fun.id))
;;

let () =
  Alcotest.run
    "Hit_testing"
    [ ( "tabs"
      , [ Alcotest.test_case
            "titles"
            `Quick
            test_a_click_on_a_title_or_its_spaces_finds_the_title
        ; Alcotest.test_case
            "other rows"
            `Quick
            test_a_click_off_the_row_of_the_tabs_finds_nothing
        ; Alcotest.test_case "block" `Quick test_the_block_moves_the_titles
        ] )
    ; ( "list"
      , [ Alcotest.test_case
            "scrolling"
            `Quick
            test_a_click_on_a_list_item_finds_it_whatever_the_scrolling
        ; Alcotest.test_case
            "empty rows"
            `Quick
            test_rows_without_items_and_columns_outside_find_nothing
        ; Alcotest.test_case
            "block"
            `Quick
            test_the_block_of_a_list_is_not_an_item
        ] )
    ; ( "table"
      , [ Alcotest.test_case
            "header"
            `Quick
            test_the_header_of_a_table_is_not_a_row
        ; Alcotest.test_case
            "scrolling"
            `Quick
            test_a_scrolled_table_shifts_the_rows
        ] )
    ; ( "properties"
      , List.map
          QCheck_alcotest.to_alcotest
          [ property_a_list_click_finds_what_is_drawn
          ; property_a_table_click_finds_what_is_drawn
          ] )
    ]
;;
